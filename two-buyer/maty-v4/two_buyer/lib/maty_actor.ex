defmodule MatyActor do
  @callback init_actor(args :: any()) :: {:ok, actor_state :: any()}

  @callback register(
              session_id :: String.t(),
              role :: atom(),
              from_pid :: pid(),
              actor_state :: any()
            ) :: {:ok, new_state :: any()}

  # @callback setup_actor(actor_state :: any()) :: {:ok, new_state :: any()}

  @callback init_session(session_id :: String.t(), actor_state :: any()) ::
              {:ok, next_fun :: function(), local_state :: any(), new_actor_state :: any()}

  # Using macro to inject default implementations
  defmacro __using__(_opts) do
    quote do
      @behaviour MatyActor

      # Default no-op for `register/4`
      @impl true
      def register(_session_id, _role, _from_pid, actor_state) do
        {:ok, actor_state}
      end

      # @impl true
      # def setup_actor(state), do: {:ok, state}

      @impl true
      def init_session(session_id, actor_state) do
        # If you prefer a no-op, do that. Or raise an error.
        {:ok, &__MODULE__.no_op_handler/2, %{}, actor_state}
      end

      def no_op_handler(_msg, local_state), do: {:done, &__MODULE__.no_op_handler/2, local_state}

      defoverridable register: 4, setup_actor: 1, init_session: 2

      # A function to spawn the actor
      def start_link(args) do
        MatyActor.start_link(__MODULE__, args)
      end
    end
  end

  # ------------------------------------------------------------------

  # The runtime logic

  @doc """
  Starts a new MatyActor process using the given module.
  Calls `module.init_actor(args)` to get initial state,
  then enters a receive loop.
  """
  def start_link(module, args) do
    pid = spawn_link(fn -> init_and_run(module, args) end)
    {:ok, pid}
  end

  defp init_and_run(module, args) do
    {:ok, actor_state} = module.init_actor(args)
    # {:ok, actor_state} = module.setup_actor(actor_state)

    loop(module, actor_state)
  end

  defp loop(module, actor_state) do
    receive do
      {:session_message, session_id, msg} ->
        {session_info, actor_state} =
          case Map.fetch(actor_state.sessions, session_id) do
            {:ok, session_info} ->
              {session_info, actor_state}

            :error ->
              # does it matter if the init_session function isn't defined?
              {:ok, next_fun, local_state, new_actor_state} =
                module.init_session(session_id, actor_state)

              session_info = %{next_handler: next_fun, local_state: local_state}

              updated_actor_state = put_in(new_actor_state, [:sessions, session_id], session_info)

              {session_info, updated_actor_state}
          end

        {action, next_fun, new_local_state} =
          apply(session_info.next_handler, [msg, session_info.local_state])

        actor_state = handle_action(action, next_fun, new_local_state, session_id, actor_state)
        loop(module, actor_state)
    end
  end

  defp handle_action(:suspend, next_fun, new_local_state, session_id, actor_state) do
    put_in(actor_state, [:sessions, session_id], %{
      next_handler: next_fun,
      local_state: new_local_state
    })
  end

  defp handle_action(:done, _next_fun, _local_state, session_id, actor_state) do
    update_in(actor_state, [:sessions], &Map.delete(&1, session_id))
  end

  defp handle_action(:noreply, next_fun, new_local_state, session_id, actor_state) do
    # same as suspend but maybe a different semantics
    # perhaps it would just return the current actor_state
    put_in(actor_state, [:sessions, session_id], %{
      next_handler: next_fun,
      local_state: new_local_state
    })
  end
end
