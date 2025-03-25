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
    case Module.get_attribute(env.module, :handler) do
      handler when not is_nil(handler) ->
        Preprocessor.process_handler_annotation(env.module, kind, name, length(args), handler)

      nil ->
        :ok
    end

    Preprocessor.process_type_annotation(env, {name, args})
    # todo: instead of saving to another module attribute, just return the errors here
    spec_errors = Module.get_attribute(env.module, :spec_errors)

    cond do
      Enum.empty?(spec_errors) -> :ok
      true -> Logger.error("There are #{length(spec_errors)} errors in this module's specs")
    end
  end

  @doc """
  Called by Hook at `@before_compile`.
  """
  def handle_before_compile(_env) do
    # attr = Module.get_attribute(env.module, :type_specs)

    # module_header =
    #   "\n-------------------- #{inspect(env.module)} -------------------"

    # display =
    #   Enum.map_join(attr, "\n\n", fn {k, v} ->
    #     "#{inspect(k)} --> \n#{inspect(v)}"
    #   end)

    # IO.puts(module_header <> "\n" <> display <> "\n")
  end

  @doc """
  Called by Hook at `@after_compile`.
  """
  def handle_after_compile(env, bytecode) do
    _ = Maty.Typechecker.Ast.throwaway()

    dbgi_map = read_debug_info!(bytecode)

    # {fn_info, :def, _meta, fn_clauses} = share_handler()

    # result_header =
    #   TC.session_typecheck_handler(
    #     env.module,
    #     fn_info,
    #     fn_clauses
    #   )

    # Logger.log(:debug, "session typechecking handler: #{inspect(result_header)}")

    _module_definitions =
      dbgi_map[:definitions]
      |> Enum.filter(fn {_, _, meta, _} -> Keyword.get(meta, :context) != Maty.Actor end)

    module_handlers =
      Module.get_attribute(env.module, :annotated_handlers) |> Enum.map(&elem(&1, 0))

    handler_definitions =
      dbgi_map[:definitions] |> Enum.filter(&(elem(&1, 0) in module_handlers))

    {fn_info, :def, _meta, fn_clauses} =
      Enum.find(handler_definitions, fn x -> elem(x, 0) == {:share_handler, 4} end)

    res =
      TC.session_typecheck_handler(
        env.module,
        fn_info,
        fn_clauses
      )

    Logger.debug("session typechecking handler: #{inspect(fn_info)}\n#{inspect(res)}")

    # for {fn_info, :def, _meta, fn_clauses} <- handler_definitions do
    #   res =
    #     TC.session_typecheck_handler(
    #       env.module,
    #       fn_info,
    #       fn_clauses
    #     )

    #   Logger.debug("session typechecking handler: #{inspect(fn_info)}\n#{inspect(res)}")
    # end

    # function_definitions = [Maty.Typechecker.Ast.lookup_price(), shipping_date]

    # for {fn_info, _kind, _meta, fn_clauses} <- function_definitions do
    #   res =
    #     TC.typecheck_function(
    #       env.module,
    #       fn_info,
    #       fn_clauses
    #     )

    #   Logger.debug("typechecking regular function: #{inspect(fn_info)}\n#{inspect(res)}")
    # end
  end

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
end
