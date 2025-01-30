defmodule MatyActor do
  alias AccessPoint

  @type session_id :: String.t()
  @type session_info :: %{next_handler: function(), local_state: any()}
  @type actor_state :: %{
          sessions: %{session_id() => session_info :: session_info()},
          ap_pid: pid()
        }

  @callback init_actor(args :: any()) :: {:ok, actor_state :: any()}

  @callback init_session(session_id :: String.t(), actor_state :: any()) ::
              {session_info :: any(), new_actor_state :: any()}

  defmacro __using__(_opts) do
    quote do
      @behaviour MatyActor

      def start_link(args) do
        MatyActor.start_link(__MODULE__, args)
      end
    end
  end

  # ------------------------------------------------------------------

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

    loop(module, actor_state)
  end

  defp loop(module, actor_state) do
    receive do
      {:maty_message, session_id, msg, from_pid} ->
        with {:ok, session_info} <- get_session(session_id, actor_state) do
          {action, next_handler_fun, new_actor_state} =
            session_info.next_handler
            |> apply([msg, from_pid, session_info, actor_state])

          updated_actor_state =
            handle_action(action, next_handler_fun, session_id, new_actor_state)

          loop(module, updated_actor_state)
        else
          _ ->
            IO.puts("[MatyActor] Session #{session_id} not found")
            loop(module, actor_state)
        end

      # assumes that no duplication of session_id will occur
      # this message arrives from the access point only
      {:session_established, session_id, session, ^actor_state.ap_pid} ->
        updated_actor_state = put_in(actor_state, [:sessions, session_id], session)
        loop(module, updated_actor_state)
    end
  end

  defp get_session(session_id, actor_state), do: Map.fetch(actor_state.sessions, session_id)

  # perhaps I should handle uninitiated sessions differently
  defp maybe_init_session(session_id, actor_state, module) do
    case Map.fetch(actor_state.sessions, session_id) do
      {:ok, session_info} ->
        {session_info, actor_state}

      :error ->
        # this doesn't necessarily mean that this session does not exist in the access point
        # it just means that we don't have knowledge of it in our actor state
        # so we should query the access point to get the session info if it exists

        # something something goes here

        # or let's just assume that the session does not exist in the access point
        # and we need to create a new session
        {session_info, new_actor_state} = module.init_session(session_id, actor_state)
        updated_actor_state = put_in(new_actor_state, [:sessions, session_id], session_info)
        {session_info, updated_actor_state}
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

  defp handle_action(:ignore, _next_fun, _new_local_state, _session_id, actor_state) do
    actor_state
  end
end
