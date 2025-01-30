defmodule TwoBuyerMaty4.MatyActor do
  @moduledoc """
  A custom behavior for multi-session Maty-style actors in Elixir.

  Each actor:
  - Initializes with `c:init_actor/1`
  - May need to `c:register/4` (or handle a registration message).
  - Receives session-based messages (`{:session_message, session_id, msg}`)
    and dispatches to `c:handle_session_message/3`.

  Internally, we store a `:sessions` map in the actor’s state:
    %{
      sessions: %{
        "session1" => %{
          next_handler: &SomeModule.install_handler/2,
          local_state: ...
        },
        ...
      }
    }
  """

  @callback init_actor(args :: any()) :: {:ok, actor_state :: any()}

  @doc """
  Called when a `{:register, session_id, role, from_pid}` message arrives.
  The default no-op does nothing, but users can override if they want to do
  something special for registration events.
  """
  @callback register(
              session_id :: String.t(),
              role :: atom(),
              from_pid :: pid(),
              actor_state :: any()
            ) ::
              {:ok, new_state :: any()}

  @callback setup_actor(actor_state :: any()) :: {:ok, new_state :: any()}

  # ------------------------------------------------------------------

  # Using macro to inject default implementations
  defmacro __using__(_opts) do
    quote do
      @behaviour MatyActor

      # Default no-op for `register/4`
      @impl true
      def register(_session_id, _role, _from_pid, actor_state) do
        {:ok, actor_state}
      end

      @impl true
      def setup_actor(state), do: {:ok, state}

      defoverridable register: 4, setup_actor: 1

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
    {:ok, actor_state} = module.setup_actor(actor_state)

    loop(module, ensure_sessions_map(actor_state))
  end

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
            :error ->
              {:ok, next_fun, local_state, updated_actor_state} =
                module.init_session(session_id, actor_state)

              session_info = %{
                next_handler: next_fun,
                local_state: local_state
              }

              updated_actor_state =
                put_in(updated_actor_state.sessions[session_id], session_info)

              {session_info, updated_actor_state}

            {:ok, s} ->
              {s, actor_state}
          end

        # 2) Apply the session’s next_handler
        {action, next_fun, new_local_state} =
          apply(session_info.next_handler, [msg, session_info.local_state])

        # 3) Update or remove the session
        actor_state = handle_action(action, next_fun, new_local_state, session_id, actor_state)
        loop(module, actor_state)

        ...
    end
  end

  # ------------------------------------------------------------------

  # Handling a session message

  defp handle_session_message(module, session_id, msg, actor_state) do
    # If this session doesn't exist yet, we can either:
    #  - Call a user-defined init for the session, or
    #  - Just store a default "install" handler, or
    #  - Let the user do something manually.

    {session_info, actor_state} =
      case Map.fetch(actor_state.sessions, session_id) do
        :error ->
          # Create a default new session
          new_session_info = %{
            # or a default
            next_handler: &module.install_handler/2,
            local_state: %{}
          }

          actor_state = put_in(actor_state.sessions[session_id], new_session_info)

          {new_session_info, actor_state}

        {:ok, session_info} ->
          {session_info, actor_state}
      end

    # Now we call the user's `handle_session_message`
    # They decide if we suspend, done, or noreply
    case module.handle_session_message(session_id, msg, actor_state) do
      {:suspend, next_fun, new_local_state, new_actor_state} ->
        # Update the session's next_handler and local_state
        updated_session_info = %{
          session_info
          | next_handler: next_fun,
            local_state: new_local_state
        }

        updated_actor_state = put_in(new_actor_state.sessions[session_id], updated_session_info)

        loop(module, updated_actor_state)

      {:done, _reason, new_local_state, new_actor_state} ->
        # The session is finished, remove from map
        updated_sessions = Map.delete(new_actor_state.sessions, session_id)

        updated_actor_state = %{new_actor_state | sessions: updated_sessions}

        # optionally do something with `new_local_state`
        loop(module, updated_actor_state)

      {:noreply, new_local_state, new_actor_state} ->
        updated_session_info = %{
          session_info
          | local_state: new_local_state
        }

        updated_actor_state = put_in(new_actor_state.sessions[session_id], updated_session_info)

        loop(module, updated_actor_state)
    end
  end
end
