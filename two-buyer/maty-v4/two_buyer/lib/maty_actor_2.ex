defmodule MatyActor2 do
  @callback init_session(session_id :: String.t(), actor_state :: any()) ::
              {:ok, next_fun :: function(), local_state :: any(), new_actor_state :: any()}

  defmacro __using__(_opts) do
    quote do
      @behaviour MatyActor

      # By default, if user doesn't override init_session,
      # we do something like "error or store a dummy"
      @impl true
      def init_session(session_id, actor_state) do
        # If you prefer a no-op, do that. Or raise an error.
        IO.puts("[MatyActor] init_session not implemented!")
        {:ok, &__MODULE__.no_op_handler/2, %{}, actor_state}
      end

      def no_op_handler(_msg, local_state), do: {:done, &__MODULE__.no_op_handler/2, local_state}

      defoverridable init_session: 2
    end
  end

  defp loop(module, actor_state) do
    receive do
      {:session_message, session_id, msg} ->
        # 1) If session_id not found, call `init_session`
        {session_info, actor_state} =
          case Map.fetch(actor_state.sessions, session_id) do
            {:ok, s} ->
              {s, actor_state}

            :error ->
              {:ok, next_fun, local_state, updated_actor_state} =
                module.init_session(session_id, actor_state)

              session_info = %{next_handler: next_fun, local_state: local_state}

              updated_actor_state = put_in(updated_actor_state.sessions[session_id], session_info)

              {session_info, updated_actor_state}
          end

        # 2) Apply the session’s next_handler
        {action, next_fun, new_local_state} =
          apply(session_info.next_handler, [msg, session_info.local_state])

        # 3) Update or remove the session
        actor_state = handle_action(action, next_fun, new_local_state, session_id, actor_state)
        loop(module, actor_state)
    end
  end

  # --------------
  # these replace the big handle_session_message function from before (I think)

  defp handle_action(:suspend, next_fun, new_local_state, session_id, actor_state) do
    put_in(actor_state.sessions[session_id], %{
      next_handler: next_fun,
      local_state: new_local_state
    })
  end

  defp handle_action(:done, _next_fun, _local_state, session_id, actor_state) do
    update_in(actor_state.sessions, &Map.delete(&1, session_id))
  end

  defp handle_action(:noreply, next_fun, new_local_state, session_id, actor_state) do
    # same as suspend but maybe a different semantics
    put_in(actor_state.sessions[session_id], %{
      next_handler: next_fun,
      local_state: new_local_state
    })
  end
end
