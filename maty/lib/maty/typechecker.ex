defmodule Maty.Typechecker do
  @moduledoc """
  This is the main public interface for Maty’s typechecking.

  - Called by `Maty.Hook` at compile-time
  - Delegates detailed checks to submodules
  """

  alias Maty.Utils
  alias Maty.Typechecker.Delta
  alias Maty.Typechecker.TCV2
  alias Maty.Typechecker.Error
  alias Maty.Typechecker.Preprocessor

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
    # todo: potentially reverse the type_specs here

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
    all_module_definitions = fetch_module_definitions!(bytecode)

    delta_M = Module.get_attribute(env.module, :delta_M)
    delta_m = Utils.Env.get_map(env.module, :delta_M)

    delta_I = Module.get_attribute(env.module, :delta_I)
    _delta_i = Utils.Env.get_map(env.module, :delta_I)

    psi = Utils.Env.get_map(env.module, :psi)

    module_init_handlers = Delta.key_set(delta_I)
    module_handlers = Delta.key_set(delta_M)

    _errors =
      for {func_id, _kind, _meta, func_clauses} <- all_module_definitions, reduce: [] do
        acc ->
          cond do
            MapSet.member?(module_handlers, func_id) ->
              {handler_name, 4} = func_id

              # Logger.info(inspect(func_clauses))
              # throw(:def_only)

              # share =
              #   TCV2.tc_expr(
              #     env.module,
              #     %{:amount => :number},
              #     %Maty.ST.SEnd{},
              #     {{:., [line: 31, column: 27], [:erlang, :/]}, [line: 31, column: 27],
              #      [{:amount, [version: 0, line: 31, column: 20], nil}, 2]}
              #   )

              # Logger.info(inspect(share))

              # throw(:naur)

              handler_M = delta_m[handler_name]
              type_signatures = psi[func_id] |> Enum.reverse()

              # myDEBUG(:init, Utils.to_func(func_id))
              # Logger.debug("boo: #{inspect(func_clauses)}", ansi_color: :yellow)

              # Logger.info("#{inspect(handler_M)}: #{inspect(type_signatures)}", ansi_color: :light_blue)

              for {clause, type_signature} <- Enum.zip(func_clauses, type_signatures) do
                # Logger.info(
                #   "\n#{}
                #   # \n#{}
                #   \n#{inspect(handler_M)}
                #   \n#{}",
                #   ansi_color: :light_blue
                # )

                res =
                  TCV2.check_wf_message_handler_clause(
                    env.module,
                    handler_name,
                    clause,
                    handler_M.st,
                    type_signature
                  )

                # res = clause

                Logger.info("[#{inspect(handler_M.function)}]: #{inspect(res)}",
                  ansi_color: :light_blue
                )

                # throw(:oops)
              end

              # throw(:oops)

              # res = TC.session_typecheck_handler(env.module, handler, func_clauses)

              # if Enum.member?(@debug, :log_res) do
              #   log_typechecking_results(func_id, res, label: "session typechecking handler")
              # end

              # acc ++ extract_errors(res)
              acc

            func_id == {:on_link, 2} ->
              # res = TC.session_typecheck_init_actor(env.module, func_id, func_clauses)

              # if Enum.member?(@debug, :log_res) do
              #   log_typechecking_results(func_id, res, label: "session typechecking on_link")
              # end

              # acc ++ extract_errors(res)
              acc

            MapSet.member?(module_init_handlers, func_id) ->
              _handler_I =
                delta_I
                |> Enum.find_value(fn {handler, map} ->
                  if map.function == func_id, do: handler
                end)

              # myDEBUG(:init, Utils.to_func(func_id))
              # Logger.debug("boo: #{inspect(func_clauses)}", ansi_color: :yellow)
              # hello4 = psi[func_id]

              # Logger.info("#{inspect(handler_I)}: #{inspect(hello4)}", ansi_color: :light_blue)

              # todo: typecheck each clause by itself
              # [
              #   {_meta, [args_ast, state_var_ast, session_ctx], [],
              #    {:try, _, [[do: {:__block__, _, [_, _, {:__block__, _, block}]}, catch: _]]}}
              # ]
              # for clause <- func_clauses do
              #   TCV2.check_wf_init_handler_clause(
              #     env.module,
              #     handler_label,
              #     clause,
              #     # # %{function: {name, arity}, st: session_type}
              #     delta_i_entry,
              #     # # List of {[arg_types], return_type} for the function
              #     psi_entry
              #   )
              # end

              # res = TC.session_typecheck_init_handler(env.module, handler_M, func_clauses)

              # if Enum.member?(@debug, :log_res) do
              #   log_typechecking_results(func_id, res, label: "session typechecking handler")
              # end

              # acc ++ extract_errors(res)
              acc

            true ->
              _well_formed = TCV2.check_wf_function(env.module, func_id, func_clauses)

              # Logger.debug("[#{Maty.Utils.to_func(func_id)}] WF-Func: #{inspect(well_formed)}")

              # res = TC.typecheck_function(env.module, func_id, func_clauses)

              # if Enum.member?(@debug, :log_res) do
              #   log_typechecking_results(func_id, res, label: "typechecking regular function")
              # end

              # acc ++ extract_errors(res)
              acc
          end
      end

    # if length(errors) != 0 do
    #   for err <- errors do
    #     Logger.error(err)
    #   end
    # else
    #   Logger.info("\n[#{env.module}] No communication errors", ansi_color: :light_green)
    # end
  end

  def fetch_module_definitions!(bytecode) do
    read_debug_info!(bytecode)
    |> Map.fetch!(:definitions)
    |> Enum.reject(fn x -> Keyword.get(elem(x, 2), :context) == Maty.Actor end)
  end

  # # Function to read debug information from bytecode.
  # #
  # # Adapted from: https://github.com/gertab/ElixirST by Gerard Tabone
  # # License: GPL-3.0 license
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

  # defp log_typechecking_results(func_id, res, label: label) do
  #   out = fn x -> "#{label}: #{inspect(func_id)}\n#{inspect(x)}" end

  #   for clause_res <- res do
  #     case clause_res do
  #       {:error, error} -> out.(error) |> Logger.error()
  #       {:ok, return} -> out.(return) |> Logger.debug()
  #     end
  #   end
  # end

  # defp extract_errors(res) do
  #   case res do
  #     {:ok, _} ->
  #       []

  #     {:error, error} ->
  #       [error]

  #     list when is_list(list) ->
  #       Enum.flat_map(list, fn
  #         {:ok, _} -> []
  #         {:error, error} -> [error]
  #       end)
  #   end
  # end

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

  def myDEBUG(num, extra \\ ""), do: Logger.debug("[#{num}] #{extra}", ansi_color: :light_blue)
end
