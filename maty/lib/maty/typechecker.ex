defmodule Maty.Typechecker do
  @moduledoc """
  This is the main public interface for Maty’s typechecking.

  - Called by `Maty.Hook` at compile-time
  - Delegates detailed checks to submodules
  """
  alias Maty.ST

  alias Maty.Typechecker.{
    SessionChecker,
    CoreChecker,
    Error,
    Tc
  }

  require Logger

  @doc """
  Called by Hook when a function definition is encountered (`@on_definition`).
  """
  def handle_on_definition(env, kind, name, args, guards, body) do
    # Possibly gather all necessary info from module attributes
    # Then delegate or store for a later pass
    # e.g. SessionChecker.check_handler_header(env, name, args, body)
    # or CoreChecker.collect_specs(...)
    SessionChecker.process_handler_annotation(env, {name, args, body})
    CoreChecker.process_type_annotation(env, {name, args, body})
  end

  @doc """
  Called by Hook at `@before_compile`.
  """
  def handle_before_compile(env) do
    # Maybe finalise or cross-check certain data here

    pairs = Module.get_attribute(env.module, :fn_st_keys)

    module_header =
      "\n-------------------- #{inspect(env.module)} -------------------"

    pairs_string =
      Enum.map_join(pairs, "\n\n", fn {k, v} ->
        "#{inspect(k)} --> \n#{inspect(v)}"
      end)

    IO.puts(module_header <> "\n" <> pairs_string <> "\n")
  end

  @doc """
  Called by Hook at `@after_compile`.
  """
  def handle_after_compile(env, bytecode) do
    dbgi_map = read_debug_info(bytecode)

    ast = SessionChecker.sample_handler()

    function_block = [
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
                {{:., [line: 71, column: 28], [TwoBuyer.Participants.Seller, :decision_handler]},
                 [no_parens: true, line: 71, column: 29], []},
                4
              ]}
           ]}, :buyer2},
         {:state, [version: 2, line: 71, column: 59], nil}
       ]}
    ]

    IO.inspect(ast)

    var_env = %{title: :binary, session: :session_ctx, state: :maty_actor_state}

    result1 =
      Tc.session_typecheck(
        env.module,
        var_env,
        %ST.SOut{
          to: :buyer1,
          message: {:quote, :number},
          continue_as: [%ST.SHandler{handler: :decision_handler}]
        },
        {:=, [line: 68, column: 12],
         [
           {:amount, [version: 3, line: 68, column: 5], nil},
           {:lookup_price, [line: 68, column: 14],
            [{:title, [version: 0, line: 68, column: 27], nil}]}
         ]}
      )

    # Logger.log(:debug, "session typechecking 1: #{inspect(result1)}")

    result2 =
      Tc.session_typecheck(
        env.module,
        %{title: :binary, session: :session_ctx, amount: :number, state: :maty_actor_state},
        %ST.SOut{
          to: :buyer1,
          message: {:quote, :number},
          continue_as: [%ST.SHandler{handler: :decision_handler}]
        },
        {:maty_send, [line: 70, column: 5],
         [
           {:session, [version: 1, line: 70, column: 15], nil},
           :buyer1,
           {:quote, {:amount, [version: 3, line: 70, column: 42], nil}}
         ]}
      )

    # Logger.log(:debug, "session typechecking 2: #{inspect(result2)}")

    result3 =
      Tc.session_typecheck(
        env.module,
        %{title: :binary, session: :session_ctx, amount: :number, state: :maty_actor_state},
        %ST.SHandler{handler: :decision_handler},
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
      )

    Logger.log(:debug, "session typechecking 3: #{inspect(result3)}")
  end

  defp read_debug_info(bytecode) do
    try do
      try do
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
