defmodule Maty.Typechecker.SessionChecker do
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
end
