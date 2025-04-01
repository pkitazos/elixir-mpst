defmodule Maty.Typechecker do
  @moduledoc """
  This is the main public interface for Maty’s typechecking.

  - Called by `Maty.Hook` at compile-time
  - Delegates detailed checks to submodules
  """

  alias Maty.Typechecker.Tc, as: TC
  alias Maty.Typechecker.{Preprocessor, Error}

  require Logger

  @doc """
  Called by Hook when a function definition is encountered (`@on_definition`).
  """
  def handle_on_definition(env, kind, name, args, _guards, _body) do
    handler = Module.get_attribute(env.module, :handler)

    if not is_nil(handler) do
      Preprocessor.process_handler_annotation(
        env.module,
        kind,
        name,
        length(args),
        handler,
        line: env.line
      )

      # todo: handle these errors properly
    end

    Preprocessor.process_type_annotation(env, {name, args})
  end

  @debug_before false

  @doc """
  Called by Hook at `@before_compile`.
  """
  def handle_before_compile(env) do
    if @debug_before do
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

    module_handlers =
      env.module
      |> Module.get_attribute(:annotated_handlers)
      |> Enum.map(&elem(&1, 0))
      |> MapSet.new()

    all_module_definitions =
      dbgi_map[:definitions]
      |> Enum.reject(fn x -> Keyword.get(elem(x, 2), :context) == Maty.Actor end)

    invalid_comm_functions =
      all_module_definitions
      |> find_invalid_comm_functions(module_handlers)
      |> MapSet.new()

    errors =
      for {fn_info, _kind, _meta, fn_clauses} <- all_module_definitions, reduce: [] do
        acc ->
          cond do
            MapSet.member?(module_handlers, fn_info) ->
              res = TC.session_typecheck_handler(env.module, fn_info, fn_clauses)
              # log_typechecking_results(fn_info, res, label: "session typechecking handler")
              acc ++ extract_errors(res)

            fn_info == {:init_actor, 1} ->
              res = TC.session_typecheck_init_actor(env.module, fn_info, fn_clauses)
              # log_typechecking_results(fn_info, res, label: "session typechecking init_actor")
              acc ++ extract_errors(res)

            MapSet.member?(invalid_comm_functions, fn_info) ->
              err = Error.non_handler_communication(fn_info)
              acc ++ [err]

            true ->
              res = TC.typecheck_function(env.module, fn_info, fn_clauses)
              # log_typechecking_results(fn_info, res, label: "typechecking regular function")
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

  defp find_invalid_comm_functions(definitions, module_handlers) do
    for {fn_info, _, _, fn_clauses} <- definitions,
        not MapSet.member?(module_handlers, fn_info),
        fn_info != {:init_actor, 1},
        clause <- fn_clauses,
        TC.performs_communication?(clause),
        reduce: [] do
      acc -> [fn_info | acc]
    end
  end

  defp show_function_signatures(module) do
    attr = Module.get_attribute(module, :type_specs)

    module_header =
      "\n-------------------- #{inspect(module)} -------------------"

    display =
      Enum.map_join(attr, "\n\n", fn {k, v} ->
        "#{inspect(k)} --> \n#{inspect(v)}"
      end)

    IO.puts(module_header <> "\n" <> display <> "\n")
  end
end
