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
        [{from, _pid}] =
          session.participants
          |> Map.to_list()
          |> Enum.filter(fn {_key, val} -> val == self() end)

        send(session.participants[to], {:maty_message, session.id, to, from, msg})
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
      {:maty_message, session_id, to, from, msg} ->
        session = actor_state.sessions[session_id]
        # to    -> my role
        # from  -> recipient role
        {handler, expected_role} = session.handlers[to]

        if from != expected_role do
          # message came from the wrong participant
          # forward to back of mailbox

          send(self(), {:maty_message, session.id, to, from, msg})
          loop(module, actor_state)
        else
          {action, next, intermediate_state} = handler.(msg, from, session, actor_state)

          updated_actor_state =
            case action do
              :suspend -> put_in(intermediate_state, [:sessions, session.id, :handlers, to], next)
              :done -> update_in(intermediate_state, [:sessions], &Map.delete(&1, session.id))
              # should I still have skip messages?
              # if this branch runs that means that we received a message from the expected participant
              # but the shape of msg was unexpected
              :continue -> intermediate_state
            end

          loop(module, updated_actor_state)
        end

      {:init_session, session_id, participants, init_token} ->
        partial_session = %{
          id: session_id,
          participants: participants,
          handlers: %{},
          local_state: %{}
        }

        initial_actor_state = put_in(actor_state, [:sessions, session_id], partial_session)

        {role, callback} = initial_actor_state.callbacks[init_token]

        {:suspend, handler_info, intermediate_state} = callback.(session_id, initial_actor_state)

        updated_actor_state =
          put_in(intermediate_state, [:sessions, session_id, :handlers, role], handler_info)

        loop(module, updated_actor_state)
    end
  end
end
