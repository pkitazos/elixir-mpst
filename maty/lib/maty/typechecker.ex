defmodule Maty.Typechecker do
  @moduledoc """
  This is the main public interface for Maty’s typechecking.

  - Called by `Maty.Hook` at compile-time
  - Delegates detailed checks to submodules
  """

  alias Maty.Typechecker.{
    SessionChecker,
    BasicChecker
  }

  @doc """
  Called by Hook when a function definition is encountered (`@on_definition`).
  """
  def handle_on_definition(env, kind, name, args, guards, body) do
    # Possibly gather all necessary info from module attributes
    # Then delegate or store for a later pass
    # e.g. SessionChecker.check_handler_header(env, name, args, body)
    # or BasicChecker.collect_specs(...)
    SessionChecker.process_handler_annotation(env, {name, args, body})
    BasicChecker.process_type_annotation(env, {name, args, body})
  end

  @doc """
  Called by Hook at `@before_compile`.
  """
  def handle_before_compile(env) do
    # Maybe finalise or cross-check certain data here

    pairs = Module.get_attribute(env.module, :pairs)

    for {k, v} <- pairs do
      IO.puts("#{inspect(k)} --> \n#{inspect(v)}\n\n")
    end

    IO.puts("-----------------------------")
  end

  @doc """
  Called by Hook at `@after_compile`.
  """
  def handle_after_compile(env, bytecode) do
    try do
      try do
        # Gets debug_info chunk from BEAM file
        chunks =
          case :beam_lib.chunks(bytecode, [:debug_info]) do
            {:ok, {_mod, chunks}} -> chunks
            {:error, _, error} -> throw({:error, inspect(error)})
          end

        # Gets the (extended) Elixir abstract syntax tree from debug_info chunk
        dbgi_map =
          case chunks[:debug_info] do
            {:debug_info_v1, :elixir_erl, metadata} ->
              case metadata do
                {:elixir_v1, map, _} ->
                  IO.inspect(map)
                  map

                {version, _, _} ->
                  throw({:error, "Found version #{version} but expected :elixir_v1."})
              end

            x ->
              throw({:error, inspect(x)})
          end
      catch
        _ -> IO.puts(:oops)
      end
    catch
      :error, error ->
        IO.puts(:oops)
    end
  end
end
