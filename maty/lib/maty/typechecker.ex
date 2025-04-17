defmodule Maty.Typechecker do
  @moduledoc """
  This is the main public interface for Maty’s typechecking.

  - Called by `Maty.Hook` at compile-time
  - Delegates detailed checks to submodules
  """

  alias Maty.Typechecker.Tc, as: TC
  alias Maty.Typechecker.{Preprocessor, Error}

  require Logger

  @debug []

  @doc """
  Called by Hook when a function definition is encountered (`@on_definition`).
  """
  def handle_on_definition(env, _kind, name, args, _guards, _body) do
    arity = length(args)

    session_types = Maty.Utils.Env.get_map(env.module, :st)
    handler = Module.get_attribute(env.module, :handler)

    if not is_nil(handler) do
      Preprocessor.process_handler_annotation(
        module: env.module,
        function: {name, arity},
        handler_label: handler,
        session_types: session_types,
        store: :delta_M,
        kind: :handler,
        meta: [line: env.line]
      )
    end

    init_handler = Module.get_attribute(env.module, :init_handler)

    if not is_nil(init_handler) do
      Preprocessor.process_handler_annotation(
        module: env.module,
        function: {name, arity},
        handler_label: init_handler,
        session_types: session_types,
        store: :delta_I,
        kind: :init_handler,
        meta: [line: env.line]
      )
    end

    Preprocessor.process_type_annotation(module: env.module, function: {name, args})
  end

  @doc """
  Called by Hook at `@before_compile`.
  """
  def handle_before_compile(env) do
    if Enum.member?(@debug, :before) do
      show_function_signatures(env.module)
    end

    spec_errors = Module.get_attribute(env.module, :spec_errors)

    if length(spec_errors) > 0 do
      out =
        for err <- spec_errors, reduce: "" do
          acc -> acc <> "#{inspect(err)}\n"
        end

      Logger.error(out)
    end
  end

  @doc """
  Called by Hook at `@after_compile`.
  """
  def handle_after_compile(env, bytecode) do
    dbgi_map = read_debug_info!(bytecode)

    delta_M = Module.get_attribute(env.module, :delta_M)
    delta_I = Module.get_attribute(env.module, :delta_I)

    module_init_handlers =
      Module.get_attribute(env.module, :delta_I)
      |> Enum.map(fn {_, x} -> x.function end)
      |> MapSet.new()

    module_handlers =
      delta_M
      |> Enum.map(fn {_, x} -> x.function end)
      |> MapSet.new()

    all_handlers = MapSet.union(module_handlers, module_init_handlers)

    all_module_definitions =
      dbgi_map[:definitions]
      |> Enum.reject(fn x -> Keyword.get(elem(x, 2), :context) == Maty.Actor end)

    invalid_comm_functions =
      all_module_definitions
      |> find_invalid_comm_functions(all_handlers)
      |> MapSet.new()

    errors =
      for {fn_info, _kind, _meta, fn_clauses} <- all_module_definitions, reduce: [] do
        acc ->
          cond do
            MapSet.member?(module_handlers, fn_info) ->
              handler =
                delta_M
                |> Enum.find_value(fn {handler, map} ->
                  if map.function == fn_info, do: handler
                end)

              res = TC.session_typecheck_handler(env.module, handler, fn_clauses)

              if Enum.member?(@debug, :log_res) do
                log_typechecking_results(fn_info, res, label: "session typechecking handler")
              end

              acc ++ extract_errors(res)

            fn_info == {:init_actor, 1} ->
              res = TC.session_typecheck_init_actor(env.module, fn_info, fn_clauses)

              if Enum.member?(@debug, :log_res) do
                log_typechecking_results(fn_info, res, label: "session typechecking init_actor")
              end

              acc ++ extract_errors(res)

            MapSet.member?(module_init_handlers, fn_info) ->
              handler_M =
                delta_I
                |> Enum.find_value(fn {handler, map} ->
                  if map.function == fn_info, do: handler
                end)

              res = TC.session_typecheck_init_handler(env.module, handler_M, fn_clauses)

              if Enum.member?(@debug, :log_res) do
                log_typechecking_results(fn_info, res, label: "session typechecking handler")
              end

              acc ++ extract_errors(res)

            MapSet.member?(invalid_comm_functions, fn_info) ->
              err = Error.non_handler_communication(fn_info)
              acc ++ [err]

            true ->
              res = TC.typecheck_function(env.module, fn_info, fn_clauses)

              if Enum.member?(@debug, :log_res) do
                log_typechecking_results(fn_info, res, label: "typechecking regular function")
              end

              acc ++ extract_errors(res)
          end
      end

    if length(errors) != 0 do
      for err <- errors do
        Logger.error(err)
      end
    else
      Logger.info("\n[#{env.module}] No communication errors", ansi_color: :light_green)
    end
  end

  # Function to read debug information from bytecode.
  #
  # Adapted from: https://github.com/gertab/ElixirST by Gerard Tabone
  # License: GPL-3.0 license
  @spec read_debug_info!(binary()) :: map() | no_return()
  defp read_debug_info!(bytecode) do
    try do
      try do
        chunks =
          case :beam_lib.chunks(bytecode, [:debug_info]) do
            {:ok, {_mod, chunks}} -> chunks
            {:error, _, error} -> throw({:error, inspect(error)})
          end

        # Gets the (extended) Elixir abstract syntax tree from debug_info chunk
        case chunks[:debug_info] do
          {:debug_info_v1, :elixir_erl, metadata} ->
            case metadata do
              {:elixir_v1, map, _} -> map
              {version, _, _} -> throw({:error, Error.version_mismatch(:elixir_v1, version)})
            end

          x ->
            throw({:error, inspect(x)})
        end
      catch
        _ -> throw({:error, :oops})
      end
    catch
      :error, error ->
        throw({:error, inspect(error)})
    end
  end

  defp log_typechecking_results(fn_info, res, label: label) do
    out = fn x -> "#{label}: #{inspect(fn_info)}\n#{inspect(x)}" end

    for clause_res <- res do
      case clause_res do
        {:error, error} -> out.(error) |> Logger.error()
        {:ok, return} -> out.(return) |> Logger.debug()
      end
    end
  end

  defp extract_errors(res) do
    case res do
      {:ok, _} ->
        []

      {:error, error} ->
        [error]

      list when is_list(list) ->
        Enum.flat_map(list, fn
          {:ok, _} -> []
          {:error, error} -> [error]
        end)
    end
  end

  defp find_invalid_comm_functions(definitions, handlers) do
    for {fn_info, _, _, fn_clauses} <- definitions,
        not MapSet.member?(handlers, fn_info),
        fn_info != {:init_actor, 1},
        clause <- fn_clauses,
        TC.performs_communication?(clause),
        reduce: [] do
      acc -> [fn_info | acc]
    end
  end

  defp show_function_signatures(module) do
    attr = Module.get_attribute(module, :psi)

    module_header =
      "\n-------------------- #{inspect(module)} -------------------"

    display =
      Enum.map_join(attr, "\n\n", fn {k, v} ->
        "#{inspect(k)} --> \n#{inspect(v)}"
      end)

    IO.puts(module_header <> "\n" <> display <> "\n")
  end
end
