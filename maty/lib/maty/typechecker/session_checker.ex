defmodule Maty.Typechecker.SessionChecker do
  alias Maty.Typechecker.CoreChecker
  alias Maty.ST, as: ST
  alias Maty.Typechecker.Error, as: Err
  alias Maty.Typechecker.Message, as: Message
  alias Maty.Utils

  @type value ::
          :any
          | :atom
          | :binary
          | :boolean
          | :date
          | :number
          | :pid
          | :string
          | :no_return
          | nil

  @type var_env() :: %{atom() => value()}

  @doc """
  This function is responsible for reading the annotation above a function @on_definition and figuring out
  a) whether this handler exists
  b) whether this function has the correct shape to be a handler
  c) if it is a handler and the associated session type has several branches, which branch should map to this handler
  d) is that even necessary

  ## Question
  what if I don't do any of these checks at this point and I just blindly save it and then using the @after_compile
  hook, I do my checks to see if this handler matches or whatever

  are these different kinds of errors and therefore should be reported at different times?
  tbd.

  """
  def process_handler_annotation(env, {name, args, body}) do
    sts = Module.get_attribute(env.module, :st) |> Enum.into(%{})
    handler = Module.get_attribute(env.module, :handler)
    Module.delete_attribute(env.module, :handler)

    if handler != nil do
      case Map.fetch(sts, handler) do
        {:ok, _pre} ->
          case typecheck_header(env, handler, args, body) do
            {:error, error} ->
              IO.puts(error)

            {:ok, {label, key}} ->
              # todo find better name for key

              arity = length(args)

              Module.put_attribute(
                env.module,
                :pairs,
                {{name, arity, label}, {handler, key}}
              )

              Utils.ModAttr.append_to_key(env, :fn_st_keys, {name, arity}, key)
              Utils.ModAttr.remove_from_key(env, :st, handler, key)
          end

        :error ->
          Message.handler_annotation(name, handler)
      end
    end
  end

  @doc """
  This one definitely feels a bit weird

  ## Question
  Do I bother with all this at this time i the compilation process or do I wait and do these checks @after_compile?
  """
  def typecheck_header(env, handler, args, _body) do
    arity = length(args)

    cond do
      arity > 4 -> IO.puts("too many args passed to handler")
      arity < 4 -> IO.puts("too few args passed to handler")
      true -> :ok
    end

    [msg, from | _] = args

    case msg do
      {label, val} ->
        all_sts = Module.get_attribute(env.module, :st) |> Enum.into(%{})
        keys = all_sts[handler]

        bs =
          for key <- keys do
            branch = ST.Lookup.get(key)
            expected_label = elem(branch.message, 0)

            cond do
              # for this first check any branch should yield the same result
              from != branch.from ->
                error =
                  Err.participant_mismatch(handler, env.line, expected: branch.from, got: from)

                {:error, error}

              label != expected_label ->
                error =
                  Err.label_mismatch(handler, env.line, expected: expected_label, got: label)

                {:error, error}

              not is_some_val(val) ->
                # todo somehow use dialyzer types
                {:error, fn -> IO.puts("Type mismatch") end}

              true ->
                {:ok, {label, key}}
            end
          end

        bs |> monad_sum()

      _ ->
        error = Err.malformed_message(handler, env.line, tuple_size: tuple_size(msg))
        {:error, error}
    end
  end

  def sample_handler() do
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

  def session_typecheck(var_env, %ST.SOut{} = pre, expr) do
    with %ST.SOut{
           to: expected_role,
           message: {expected_label, expected_payload},
           continue_as: continue_as
         } <- pre do
      {:maty_send, _, [session_ctx, role, {label, payload}]} = expr

      session_ctx_type = CoreChecker.get_type(session_ctx, var_env)
      expected_payload_type = CoreChecker.get_type(expected_payload, var_env)
      payload_type = CoreChecker.get_type(payload, var_env)

      cond do
        session_ctx_type != :session_ctx -> {:error, :invalid_session_ctx}
        expected_role != role -> {:error, :sending_to_wrong_role}
        expected_label != label -> {:error, :sending_wrong_label}
        expected_payload_type != payload_type -> {:error, :sending_payload_of_wrong_type}
        true -> {:ok, {continue_as}}
      end
    else
      # that's like the only thing you're allowed to do
      _ -> {:error, "you're supposed to be sending a message now"}
    end
  end

  # if my regular typecheck functions come across either of these patterns
  # I should just ignore it as neither of them mutates the state
  # whereas if my session typecheck functions come across any other pattern
  # they should ignore it as it doesn't move the session forward

  def session_typecheck(var_env, %ST.SEnd{} = _pre, expr) do
    with {:{}, _,
          [
            :suspend,
            {{:&, _, [{:/, [], [{{:., _, [module, handler]}, _, []}, arity]}]}, role},
            state
          ]} <-
           expr do
      state_type = CoreChecker.get_type(state, var_env)
      role_type = CoreChecker.get_type(role, var_env)

      # todo
      # - lookup handler (check that it exists)
      # - make sure it exists in this module? (Elixir may already take care of this for us)
      # - compare handler to all continuations in branch

      cond do
        state_type != :maty_actor_type -> {:error, "state is not valid maty_actor_state"}
        # ? how can I allow `@role` to be used
        role_type != :atom -> {:error, "invalid type for role"}
        arity != 4 -> {:error, "this is not a valid handler"}
      end
    else
      _ -> {:error, "if this is the last expression in the function it should suspend here"}
    end
  end

  # sessionTypechecker :: Env -> ST -> Expr -> Either String (Maybe (Type, ST))
  def session_typechecker(var_env, pre, expr) do
    case CoreChecker.get_type(expr, var_env) do
      :error -> {:error, "some error message"}
      type -> {:ok, {type, pre}}
    end
  end

  # replace with proper typechecking functions

  defp is_var({tag, ctx, children}), do: is_atom(tag) and is_list(ctx) and is_nil(children)
  defp is_var(_), do: false

  defp is_some_val(var) do
    # todo make this list exhaustive
    is_var(var) or
      is_atom(var) or
      is_binary(var) or
      is_number(var) or
      is_bitstring(var) or
      is_boolean(var)
  end

  # defp extract_body(do: {:__block__, [], block}), do: block
  # defp extract_body(do: expr), do: expr

  defp monad_sum(bs) do
    case Enum.find(bs, fn {atom, _} -> atom == :ok end) do
      {:ok, id} ->
        {:ok, id}

      _ ->
        # * handle different errors
        # may be a non-issue once I start consuming session types from my annotations
        List.first(bs)
    end
  end
end
