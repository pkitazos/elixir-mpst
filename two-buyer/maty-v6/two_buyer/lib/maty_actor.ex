defmodule MV6.MatyActor do
  @callback init_actor(args :: any()) :: {:ok, actor_state :: any()}

  defmacro __using__(_opts) do
    quote do
      @behaviour MV6.MatyActor

      def start_link(args) do
        MV6.MatyActor.start_link(__MODULE__, args)
      end

      def maty_send(from, to, session_id, msg) do
        send(to, {:maty_message, session_id, msg, from})
      end

      def register(ap_pid, role, callback, state) do
        init_token = make_ref()
        send(ap, {:register, role, self(), init_token})

        updated_state = %{state | callbacks: Map.put(state.callbacks, init_token, callback)}
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

  defp loop(
         module,
         %{ap_pid: ap, role: role, sessions: sessions, callbacks: callbacks} = actor_state
       ) do
    receive do
      {:maty_message, session_id, msg, from_pid} ->
        {:ok, session_info} <- Map.fetch!(actor_state.sessions, session_id)

        {action, next_handler_fun, new_actor_state} =
          session_info.next_handler
          |> apply([msg, from_pid, session_info, actor_state])

        updated_actor_state =
          handle_action(action, next_handler_fun, session_id, new_actor_state)

        loop(module, updated_actor_state)

      {:init_session, session_id, participants, role, init_token, ^ap} ->
        callback = Map.fetch!(callbacks, init_token)
        {:suspend, initial_handler, _actor_state} = callback.()

        session_info = %{
          id: session_id,
          participants: participants,
          next_handler: initial_handler,
          local_state: %{}
        }

        updated_actor_state = %{
          actor_state
          | sessions: Map.put(actor_state.sessions, session_id, session_info)
        }

        loop(module, updated_actor_state)
    end
  end

  defp handle_action(:suspend, next_fun, session_id, actor_state) do
    put_in(actor_state, [:sessions, session_id, :next_handler], next_fun)
  end

  defp handle_action(:done, :unit, session_id, actor_state) do
    update_in(actor_state, [:sessions], &Map.delete(&1, session_id))
  end

  defp handle_action(:continue, nil, _session_id, actor_state) do
    actor_state
  end
end
