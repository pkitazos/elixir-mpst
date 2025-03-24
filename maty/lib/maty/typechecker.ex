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
    dbgi_map = read_debug_info!(bytecode)

    # IO.inspect(dbgi_map[:definitions])

    {fn_info, _kind, _meta0, fn_clauses} = sample_handler()

    result_header =
      TC.session_typecheck_handler(
        env.module,
        fn_info,
        fn_clauses
      )

    Logger.log(:debug, "session typechecking handler: #{inspect(result_header)}")

    _module_definitions =
      dbgi_map[:definitions]
      |> Enum.filter(fn {_, _, meta, _} -> Keyword.get(meta, :context) != Maty.Actor end)
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

  defp sample_handler() do
    {{:title_handler, 4}, :def, [line: 67, column: 7],
     [
       {[line: 67, column: 7],
        [
          {:title, {:title, [version: 0, line: 67, column: 30], nil}},
          :buyer1,
          {:session, [version: 1, line: 67, column: 47], nil},
          {:state, [version: 2, line: 67, column: 56], nil}
        ], [],
        {:__block__, [],
         [
           {:=, [line: 68, column: 12],
            [
              {:amount, [version: 3, line: 68, column: 5], nil},
              {:lookup_price, [line: 68, column: 14],
               [{:title, [version: 0, line: 68, column: 27], nil}]}
            ]},
           {:maty_send, [line: 70, column: 5],
            [
              {:session, [version: 1, line: 70, column: 15], nil},
              :buyer1,
              {:quote, {:amount, [version: 3, line: 70, column: 42], nil}}
            ]},
           {:{}, [line: 71, column: 5],
            [
              :suspend,
              {{:&, [line: 71, column: 17],
                [
                  {:/, [],
                   [
                     {{:., [line: 71, column: 28],
                       [TwoBuyer.Participants.Seller, :decision_handler]},
                      [no_parens: true, line: 71, column: 29], []},
                     4
                   ]}
                ]}, :buyer2},
              {:state, [version: 2, line: 71, column: 59], nil}
            ]}
         ]}}
     ]}
  end
end
