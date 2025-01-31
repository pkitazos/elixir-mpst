defmodule TwoBuyerMaty5.MatyActor do
  alias TwoBuyerMaty5.AccessPoint

  @type session_id :: String.t()
  @type session_context :: %{any() => pid()}
  @type session_info :: %{
          id: session_id(),
          next_handler: function(),
          participants: session_context(),
          local_state: any()
        }
  @type actor_state :: %{
          sessions: %{session_id() => session_info :: session_info()},
          ap_pid: pid()
        }

  @callback init_actor(args :: any()) :: {:ok, actor_state :: any()}

  @callback init_session(session_id :: String.t(), actor_state :: any()) ::
              {session_info :: any(), new_actor_state :: any()}

  defmacro __using__(_opts) do
    quote do
      @behaviour TwoBuyerMaty5.MatyActor

      def start_link(args) do
        TwoBuyerMaty5.MatyActor.start_link(__MODULE__, args)
      end

      def maty_send(to, session_id, msg), do: send(to, {:maty_message, session_id, msg, self()})

      def fetch_session(pid, session_id) do
        send(pid, {:fetch_session, session_id, pid})
        IO.puts("[DEBUG] fetching session #{session_id}")
        :ok
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
    ap = actor_state.ap_pid
    IO.puts("[MatyActor] Waiting for messages")

    receive do
      {:maty_message, session_id, msg, from_pid} ->
        IO.puts("[MatyActor] Received message #{inspect(msg)} from #{inspect(from_pid)}")

        hello = get_session(session_id, actor_state)
        IO.puts("[DEBUG] hello: #{inspect(hello)}")

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

      {:register, session_id, role, caller} ->
        AccessPoint.register(ap, session_id, role, caller)
        loop(module, actor_state)

      {:fetch_session, session_id, caller} ->
        AccessPoint.fetch_session(ap, session_id, caller)
        loop(module, actor_state)

      # assumes that no duplication of session_id will occur
      # this message arrives from the access point only
      {:session_participants, session_id, participants, ^ap} ->
        IO.puts("[MatyActor] Received session participants: #{inspect(participants)}")

        updated_actor_state =
          actor_state
          |> put_in([:sessions], %{session_id => %{participants: participants}})
          # may be redundant
          |> put_in([:sessions, session_id, :id], session_id)

        # updated_actor_state =
        #   actor_state
        #   |> put_in([:sessions, session_id, :participants], participants)
        #   # may be redundant
        #   |> put_in([:sessions, session_id, :id], session_id)

        loop(module, updated_actor_state)
    end
  end

  defp handle_action(:suspend, next_fun, session_id, actor_state) do
    put_in(actor_state, [:sessions, session_id, :next_handler], next_fun)
  end

  defp handle_action(:done, _next_fun, session_id, actor_state) do
    update_in(actor_state, [:sessions], &Map.delete(&1, session_id))
  end

  defp handle_action(:continue, _next_fun, _session_id, actor_state) do
    actor_state
  end

  defp get_session(session_id, actor_state), do: Map.fetch(actor_state.sessions, session_id)
end
