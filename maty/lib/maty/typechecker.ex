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
      Preprocessor.process_handler_annotation(env.module, kind, name, length(args), handler)
    end

    Preprocessor.process_type_annotation(env, {name, args})
  end

  @doc """
  Called by Hook at `@before_compile`.
  """
  def handle_before_compile(env) do
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
      Module.get_attribute(env.module, :annotated_handlers) |> Enum.map(&elem(&1, 0))

    all_module_definitions =
      dbgi_map[:definitions]
      |> Enum.filter(fn {_, _, meta, _} -> Keyword.get(meta, :context) != Maty.Actor end)

    for {fn_info, _kind, _meta, fn_clauses} <- all_module_definitions do
      cond do
        fn_info in module_handlers ->
          res = TC.session_typecheck_handler(env.module, fn_info, fn_clauses)
          Logger.debug("session typechecking handler: #{inspect(fn_info)}\n#{inspect(res)}")

        fn_info == {:init_actor, 1} ->
          Logger.debug("this is the init_actor function:")

        true ->
          res = TC.typecheck_function(env.module, fn_info, fn_clauses)
          Logger.debug("typechecking regular function: #{inspect(fn_info)}\n#{inspect(res)}")
      end
    end
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
