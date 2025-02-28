defmodule Maty.Actor do
  alias Maty.Types

  @callback init_actor(args :: any()) :: {:ok, Types.maty_actor_state()}

  defmacro __using__(_opts) do
    quote do
      @behaviour Maty.Actor

      def start_link(args) do
        Maty.Actor.start_link(__MODULE__, args)
      end

      # so let's just imagine that the maty_send function gets passed the current role
      # into its context somehow
      # in that case, it's super easy to just forward this role to the event loop
      # and in-fact that's exactly what we're doing here
      # we can imagine that later on this won't be a tuple and we'll actually hide this info inside the session context
      def maty_send({session, current_role}, to, msg) do
        send(session.participants[to], {:maty_message, session.id, to, current_role, msg})
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

  @spec start_link(module(), any()) :: {:ok, pid()}
  def start_link(module, args) do
    pid = spawn_link(fn -> init_and_run(module, args) end)

    {:ok, pid}
  end

  @spec init_and_run(module(), any()) :: no_return()
  defp init_and_run(module, args) do
    {:ok, actor_state} = module.init_actor(args)

    loop(module, actor_state)
  end

  @spec loop(module(), Types.maty_actor_state()) :: no_return()
  defp loop(module, actor_state) do
    receive do
      {:maty_message, session_id, to, from, msg} ->
        # just as before, we receive the session_id, role_me and role_them
        session = actor_state.sessions[session_id]
        # to    -> my role
        # from  -> recipient role
        {handler, expected_role} = session.handlers[to]
        # can i do this with a where clause

        # here we re-route messages (busy-waiting) that came from the wrong participant back to the end of our mailbox
        # and because we do this, we don't need the continue messages anymore
        if from != expected_role do
          # message came from the wrong participant
          # forward to back of mailbox

          send(self(), {:maty_message, session.id, to, from, msg})
          loop(module, actor_state)
        else
          # pass in my role here
          # this is the key insight, we know that we want the handler to know who they're acting as
          # we onbviouslt know that this actor is currently acting as role_me
          # so we can tell the handler that in this context you are in-fact role_me
          # we'd do something like session = { session | current_role: role_me } or something
          # and then we now have access to this information everywhere that matters: the handler itself, the maty_send, etc.
          {action, next, intermediate_state} = handler.(msg, from, {session, to}, actor_state)

          updated_actor_state =
            case action do
              :suspend -> put_in(intermediate_state, [:sessions, session.id, :handlers, to], next)
              :done -> update_in(intermediate_state, [:sessions], &Map.delete(&1, session.id))
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

        {:suspend, handler_info, intermediate_state} =
          callback.({session_id, role}, initial_actor_state)

        updated_actor_state =
          put_in(intermediate_state, [:sessions, session_id, :handlers, role], handler_info)

        loop(module, updated_actor_state)
    end
  end
end
