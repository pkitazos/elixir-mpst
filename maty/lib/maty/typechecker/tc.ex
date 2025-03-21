defmodule Maty.Typechecker.Tc do
  alias Maty.ST

  require Logger

  @type value ::
          :any
          | :atom
          | :binary
          | :boolean
          | :date
          | nil
          | :number
          | :no_return
          | :pid
          | :reference
          | :unit

  @type ast :: Macro.t()

  @type var_env() :: %{atom() => value()}

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

  def comments do
    # def session_typecheck(var_env, %ST.SOut{} = pre, expr) do
    #   with %ST.SOut{
    #          to: expected_role,
    #          message: {expected_label, expected_payload},
    #          continue_as: continue_as
    #        } <- pre do
    #     {:maty_send, _, [session_ctx, role, {label, payload}]} = expr

    #     session_ctx_type = CoreChecker.get_type(session_ctx, var_env)
    #     expected_payload_type = CoreChecker.get_type(expected_payload, var_env)
    #     payload_type = CoreChecker.get_type(payload, var_env)

    #     cond do
    #       session_ctx_type != :session_ctx -> {:error, :invalid_session_ctx}
    #       expected_role != role -> {:error, :sending_to_wrong_role}
    #       expected_label != label -> {:error, :sending_wrong_label}
    #       expected_payload_type != payload_type -> {:error, :sending_payload_of_wrong_type}
    #       true -> {:ok, {continue_as}}
    #     end
    #   else
    #     # that's like the only thing you're allowed to do
    #     _ -> {:error, "you're supposed to be sending a message now"}
    #   end
    # end

    # if my regular typecheck functions come across either of these patterns
    # I should just ignore it as neither of them mutates the state
    # whereas if my session typecheck functions come across any other pattern
    # they should ignore it as it doesn't move the session forward
    # def session_typecheck(var_env, %ST.SEnd{} = _pre, expr) do
    #   with {:{}, _,
    #         [
    #           :suspend,
    #           {{:&, _, [{:/, [], [{{:., _, [module, handler]}, _, []}, arity]}]}, role},
    #           state
    #         ]} <-
    #          expr do
    #     state_type = CoreChecker.get_type(state, var_env)
    #     role_type = CoreChecker.get_type(role, var_env)

    #     # todo
    #     # - lookup handler (check that it exists)
    #     # - make sure it exists in this module? (Elixir may already take care of this for us)
    #     # - compare handler to all continuations in branch

    #     cond do
    #       state_type != :maty_actor_type -> {:error, "state is not valid maty_actor_state"}
    #       # ? how can I allow `@role` to be used
    #       role_type != :atom -> {:error, "invalid type for role"}
    #       arity != 4 -> {:error, "this is not a valid handler"}
    #     end
    #   else
    #     _ -> {:error, "if this is the last expression in the function it should suspend here"}
    #   end
    # end

    # def session_typecheck(var_env, pre, expr) do
    #   case CoreChecker.get_type(expr, var_env) do
    #     :error -> {:error, "some error message"}
    #     type -> {:ok, {type, pre}}
    #   end
    # end
  end

  # tcExpr :: Env -> ST -> Expr -> Either String (Maybe (Type, ST))
  @spec session_typecheck(module(), var_env(), ST.t(), ast()) ::
          {:error, binary(), var_env()}
          | {:ok, {:just, {value(), ST.t()}}, var_env()}
          | {:ok, :nothing, var_env()}

  # tcVal :: Env -> Val -> Either String Type
  @spec typecheck(var_env(), ast()) :: {:ok, value(), var_env()} | {:error, binary(), var_env()}
  def typecheck(var_env, :unit), do: {:ok, :unit, var_env}
  def typecheck(var_env, nil), do: {:ok, nil, var_env}
  def typecheck(var_env, val) when is_boolean(val), do: {:ok, :boolean, var_env}
  def typecheck(var_env, val) when is_atom(val), do: {:ok, :atom, var_env}
  def typecheck(var_env, val) when is_binary(val), do: {:ok, :binary, var_env}
  def typecheck(var_env, val) when is_number(val), do: {:ok, :number, var_env}
  def typecheck(var_env, val) when is_pid(val), do: {:ok, :pid, var_env}
  def typecheck(var_env, val) when is_reference(val), do: {:ok, :reference, var_env}
  def typecheck(var_env, %Date{}), do: {:ok, :date, var_env}

  def typecheck(var_env, {var, _meta, ctx}) when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) do
    case Map.fetch(var_env, var) do
      {:ok, type} -> {:ok, type, var_env}
      :error -> {:error, "variable doesn't exist", var_env}
    end
  end

  # def session_typecheck_handler(module, var_env, {{name, 4}, :def, _meta, clauses}) do
  #   st_pairs = Module.get_attribute(module, :pairs) |> Enum.into(%{})

  #   for {_meta1, [{expected_label, payload}, expected_role, session_ctx, maty_actor_state], _,
  #        block} <-
  #         clauses do
  #     case Map.fetch(st_pairs, {name, 4, expected_label}) do
  #       {:ok, {handler, st_key}} ->
  #         st = ST.Lookup.get(st_key)

  #         # %Maty.ST.SIn{
  #         #   from: :buyer1,
  #         #   message: {:title, :string},
  #         #   continue_as: [
  #         #     %Maty.ST.SOut{
  #         #       to: :buyer1,
  #         #       message: {:quote, :number},
  #         #       continue_as: [%Maty.ST.SHandler{handler: :decision_handler}]
  #         #     }
  #         #   ]
  #         # }

  #         cond do
  #           st.from != expected_role -> {:error, "role mismatch"}
  #           elem(st.msg, 0) != expected_label -> {:error, "message label mismatch"}
  #         end

  #         {:ok, "something something"}

  #       other ->
  #         Logger.error("Unexpected return from session_typecheck: #{inspect(other)}")
  #     end
  #   end
  # end

  def session_typecheck_block(module, var_env, st, expressions) when is_list(expressions) do
    Enum.reduce_while(expressions, {:ok, st, var_env}, fn expr, {:ok, current_st, current_env} ->
      case session_typecheck(module, current_env, current_st, expr) do
        {:ok, {:just, {_, new_st}}, new_env} ->
          {:cont, {:ok, new_st, new_env}}

        {:ok, :nothing, new_env} ->
          {:halt, {:ok, :nothing, new_env}}

        {:error, error_msg, new_env} ->
          {:halt, {:error, error_msg, new_env}}

        other ->
          Logger.error("Unexpected return from session_typecheck: #{inspect(other)}")

          {:halt, other}
      end
    end)
  end

  def session_typecheck(
        module,
        var_env,
        %ST.SOut{
          to: expected_role,
          message: {expected_label, expected_payload},
          continue_as: continue_as
        },
        {:maty_send, _meta, [session, role, {label, payload}]}
      )
      when is_atom(role) and is_atom(label) do
    with {:ok, :session_ctx, _} <- typecheck(var_env, session),
         {:ok, ^expected_payload, _} <- typecheck(var_env, payload) do
      cond do
        expected_role != role -> {:error, "role mismatch", var_env}
        expected_label != label -> {:error, "label mismatch", var_env}
        # ! hd() call here should go
        true -> {:ok, {:just, {nil, hd(continue_as)}}, var_env}
      end
    else
      other ->
        Logger.error("Unexpected return from session_typecheck ST.SOut: #{inspect(other)}")
        {:error, "something went wrong", var_env}
    end
  end

  def session_typecheck(
        module,
        var_env,
        %ST.SHandler{handler: handler} = st,
        {:{}, _meta, [:suspend, {fun_capture, expected_role}, state_ast]}
      )
      when is_atom(expected_role) do
    st_map = Module.get_attribute(module, :fn_st_keys) |> Enum.into(%{})

    case fun_capture do
      {:&, _meta1, [{:/, _meta2, [{{:., _meta3, [mod, fun_name]}, _meta4, _}, 4]}]} ->
        # explicit module form: &Module.fun/arity
        with {:ok, :maty_actor_state, var_env} <- typecheck(var_env, state_ast) do
          case Map.fetch(st_map, {fun_name, 4}) do
            {:ok, st_keys} ->
              correct_handler_role? =
                Enum.map(st_keys, &ST.Lookup.get/1)
                |> Enum.any?(&(&1.from == expected_role))

              cond do
                mod != module -> {:error, "Handler function from unexpected module", var_env}
                not correct_handler_role? -> {:error, "handler role mismatch", var_env}
                true -> {:ok, :nothing, var_env}
              end

            _ ->
              {:error, "this function doesn't seem to have a session type stored"}
          end
        else
          other ->
            Logger.error(
              "Unexpected return from session_typecheck ST.SHandler: #{inspect(other)}"
            )

            {:error, "something went wrong and I'm not sure what yet", var_env}
        end

      {:&, _meta1, [{:/, _meta2, [fun_name, 4]}]} ->
        # implicit module form: &fun/arity
        with {:ok, :maty_actor_state, var_env} <- typecheck(var_env, state_ast),
             {:ok, st_keys} <- Map.fetch(st_map, {fun_name, 4}) do
          correct_handler_role? =
            Enum.map(st_keys, &ST.Lookup.get/1)
            |> Enum.map(fn %ST.SIn{from: from} -> from end)
            |> Enum.any?(&(&1 == expected_role))

          cond do
            correct_handler_role? -> {:ok, :nothing, var_env}
            true -> {:error, "handler role mismatch", var_env}
          end
        else
          other ->
            Logger.error(
              "Unexpected return from session_typecheck ST.SHandler: #{inspect(other)}"
            )

            {:error, "something went not well", var_env}
        end
    end
  end

  def session_typecheck(module, var_env, st, {:=, _meta, [lhs, rhs]}) do
    with {:ok, {:just, {rhs_type, new_st}}, var_env} <-
           session_typecheck(module, var_env, st, rhs) do
      case lhs do
        {var, _meta, context} when is_atom(var) and (is_atom(context) or is_nil(context)) ->
          var_env = Map.put(var_env, var, rhs_type)

          {:ok, {:just, {rhs_type, new_st}}, var_env}

        _ ->
          {:error, "Invalid left-hand side in assignment", var_env}
      end
    else
      {:ok, :nothing} ->
        {:ok, :nothing, var_env}

      {:error, err} ->
        {:error, err, var_env}

      other ->
        Logger.error("Unexpected return from session_typecheck match operator: #{inspect(other)}")
    end
  end

  def session_typecheck(module, var_env, st, {name, _meta, args})
      when is_atom(name) and is_list(args) do
    arity = length(args)
    func_types = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    with {:ok, types} <- Map.fetch(func_types, {name, arity}) do
      # lookup the type of the function in the module annotations
      # we get back a list of tuples where the first element is the param types (which is itself a list)
      # and the second element is the return type
      # I will then check to see if the types of all the args match the types of all the params
      # if they do, then we return the corresponding return type and leave the session unchanged
      # if they don't all match then we move on to the next branch
      # if no branches match then we return an error saying these args cannot be passed to this function
      # we can also error early by checking that the args and arity match

      clause_results =
        for {param_types, return_type} <- types do
          # the fact that I'm doing this before the cond-do expression kinda defeats the purpose

          params_type_errors =
            Enum.zip_with(param_types, args, fn p, a ->
              case typecheck(var_env, a) do
                {:ok, ^p, _var_env} -> true
                _ -> false
              end
            end)
            |> Enum.filter(&(not &1))

          cond do
            length(param_types) != arity ->
              {:error, "not enough params"}

            length(params_type_errors) != 0 ->
              {:error, "mismatch between param and arg types"}

            true ->
              {:ok, return_type}
          end
        end

      return =
        Enum.find(clause_results, {:error, "something went wrong"}, fn {status, _} ->
          status == :ok
        end)

      case return do
        {:ok, some_type} -> {:ok, {:just, {some_type, st}}, var_env}
        {:error, some_error} -> {:error, some_error, var_env}
        other -> {:error, "This should be unreachable", var_env}
      end
    else
      :error -> {:error, "function doesn't seem to have a type", var_env}
    end
  end
end
