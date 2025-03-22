defmodule Maty.Typechecker do
  @moduledoc """
  This is the main public interface for Maty’s typechecking.

  - Called by `Maty.Hook` at compile-time
  - Delegates detailed checks to submodules
  """
  alias Maty.ST

  alias Maty.Typechecker.Tc, as: TC

  alias Maty.Typechecker.{
    Preprocessor,
    SessionChecker,
    Error
  }

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
  end

  @doc """
  Called by Hook at `@before_compile`.
  """
  def handle_before_compile(env) do
    attr = Module.get_attribute(env.module, :type_specs)

    module_header =
      "\n-------------------- #{inspect(env.module)} -------------------"

    display =
      Enum.map_join(attr, "\n\n", fn {k, v} ->
        "#{inspect(k)} --> \n#{inspect(v)}"
      end)

    IO.puts(module_header <> "\n" <> display <> "\n")
  end

  @doc """
  Called by Hook at `@after_compile`.
  """
  def handle_after_compile(env, bytecode) do
    _dbgi_map = read_debug_info!(bytecode)

    _ast = SessionChecker.sample_handler()

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

    var_env = %{title: :binary, session: :session_ctx, state: :maty_actor_state}

    pre_st = %ST.SOut{
      to: :buyer1,
      branches: [
        %ST.SBranch{
          label: :quote,
          payload: :number,
          continue_as: %ST.SName{handler: :decision_handler}
        }
      ]
    }

    result1 =
      TC.session_typecheck(
        env.module,
        var_env,
        pre_st,
        {:=, [line: 68, column: 12],
         [
           {:amount, [version: 3, line: 68, column: 5], nil},
           {:lookup_price, [line: 68, column: 14],
            [{:title, [version: 0, line: 68, column: 27], nil}]}
         ]}
      )

    # Logger.log(:debug, "session typechecking 1: #{inspect(result1)}")

    var_env_1 = %{
      session: :session_ctx,
      state: :maty_actor_state,
      title: :binary,
      amount: :number
    }

    pre_st_1 = %ST.SOut{
      to: :buyer1,
      branches: [
        %ST.SBranch{
          label: :quote,
          payload: :number,
          continue_as: %ST.SName{handler: :decision_handler}
        }
      ]
    }

    {:ok, {:just, {:number, ^pre_st_1}}, ^var_env_1} = result1

    result2 =
      TC.session_typecheck(
        env.module,
        var_env_1,
        pre_st_1,
        {:maty_send, [line: 70, column: 5],
         [
           {:session, [version: 1, line: 70, column: 15], nil},
           :buyer1,
           {:quote, {:amount, [version: 3, line: 70, column: 42], nil}}
         ]}
      )

    # Logger.log(:debug, "session typechecking 2: #{inspect(result2)}")

    var_env_2 = var_env_1

    pre_st_2 = %ST.SName{handler: :decision_handler}

    {:ok, {:just, {nil, ^pre_st_2}}, ^var_env_2} = result2

    result3 =
      TC.session_typecheck(
        env.module,
        var_env_2,
        pre_st_2,
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

    # Logger.log(:debug, "session typechecking 3: #{inspect(result3)}")

    var_env_3 = var_env_2

    {:ok, :nothing, ^var_env_3} = result3

    resultN =
      TC.session_typecheck_block(
        env.module,
        var_env,
        pre_st,
        function_block
      )

    # Logger.log(:debug, "session typechecking block: #{inspect(resultN)}")

    var_env_N = var_env_3

    {:ok, :nothing, ^var_env_N} = resultN

    # resultX = TC.session_typecheck_handler(env.module, %{}, ast)

    # Logger.log(:debug, "session typechecking: #{inspect(resultX)}")
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
