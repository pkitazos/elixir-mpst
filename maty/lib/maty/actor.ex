defmodule Maty.Actor do
  @callback init_actor(args :: any()) :: {:ok, actor_state :: any()}

  defmacro __using__(_opts) do
    quote do
      @behaviour Maty.Actor

      def start_link(args) do
        Maty.Actor.start_link(__MODULE__, args)
      end

      def maty_send(session, to, msg) do
        # really this should be the role of this actor in this given interaction
        # what this function does is assume you only ever have one role in a given session
        # if we have multiple roles in the same session it errors
        # in reality we want to allow an actor to have multiple roles in the same session
        from = get_role_in_interaction!(session)

        send(session.participants[to], {:maty_message, session, from, msg})
      end

      def register(ap_pid, role, callback, state) do
        init_token = make_ref()
        send(ap_pid, {:register, role, self(), init_token})

        updated_state = put_in(state, [:callbacks, init_token], {role, callback})
        {:ok, updated_state}
      end
    end
  end

  # ------------------------------------------------------------------

  def start_link(module, args) do
    pid = spawn_link(fn -> init_and_run(module, args) end)

    {:ok, pid}
  end

  defp init_and_run(module, args) do
    {:ok, actor_state} = module.init_actor(args)

    loop(module, actor_state)
  end

  defp loop(module, actor_state) do
    receive do
      {:maty_message, session, from, msg} ->
        my_role = get_role_in_interaction!(session)

        {recipient_role, handler} = session.handlers[my_role]

        if from != recipient_role do
          # message came from the wrong participant
          # forward to back of mailbox

          send(self(), {:maty_message, session, from, msg})
          loop(module, actor_state)
        else
          # message came from the right participant and I can process it just fine
          # the problem remains, what is my role in this interaction

          updated_actor_state = handler.(msg, from, session, actor_state) |> update_state(session)
          loop(module, updated_actor_state)
        end

      {:init_session, session_id, session_participants, init_token} ->
        partial_session = %{
          id: session_id,
          participants: session_participants,
          next_handler: nil,
          local_state: %{}
        }

        initial_actor_state = put_in(actor_state, [:sessions, session_id], partial_session)

        {role, callback} = initial_actor_state.callbacks[init_token]

        {:suspend, initial_handler, intermediate_actor_state, role} =
          callback.(session_id, initial_actor_state)

        updated_actor_state =
          put_in(
            intermediate_actor_state,
            [:sessions, session_id, :next_handler],
            initial_handler
          )

        loop(module, updated_actor_state)
    end
  end

  defp update_state({:suspend, {next_role, next_fun}, actor_state}, session) do
    my_role = get_role_in_interaction!(session)
    put_in(actor_state, [:sessions, session.id, :handlers, my_role], next_fun)
  end

  defp update_state({:done, :unit, actor_state}, session) do
    update_in(actor_state, [:sessions], &Map.delete(&1, session.id))
  end

  defp update_state({:continue, nil, actor_state}, _session) do
    actor_state
  end

  defp get_role_in_interaction!(session) do
    [{role, _pid}] =
      session.participants
      |> Map.to_list()
      |> Enum.filter(fn {_key, val} -> val == self() end)

    role
  end
end
