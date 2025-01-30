defmodule AccessPoint do
  alias SessionContext

  def start_link() do
    initial_state = %{
      sessions: %{},
      next_session_id: 1
    }

    pid = spawn_link(fn -> loop(initial_state) end)
    {:ok, pid}
  end

  def create_session(ap_pid) do
    send(ap_pid, {:create_session, self()})
  end

  def register(ap_pid, session_id, role, pid \\ self()) do
    send(ap_pid, {:register, session_id, role, pid})
    {:ok, session_id}
  end

  def fetch_session(ap_pid, session_id, caller \\ self()) do
    send(ap_pid, {:fetch_session, session_id, caller})
  end

  defp loop(state) do
    receive do
      # in our case the requester would only ever be the seller
      {:create_session, requester} ->
        session_id = "session#{state.next_session_id}"
        new_session = %SessionContext{}
        new_sessions = Map.put(state.sessions, session_id, new_session)

        new_state = %{state | sessions: new_sessions, next_session_id: state.next_session_id + 1}

        send(requester, {:ok, session_id})
        loop(new_state)

      {:register, session_id, role, pid} ->
        sessions_map = state.sessions
        session = Map.fetch!(sessions_map, session_id)

        updated_session =
          case role do
            :seller -> %SessionContext{session | seller: pid}
            :buyer1 -> %SessionContext{session | buyer1: pid}
            :buyer2 -> %SessionContext{session | buyer2: pid}
          end

        updated_sessions = Map.put(sessions_map, session_id, updated_session)
        loop(%{state | sessions: updated_sessions})

      {:fetch_session, session_id, caller} ->
        case Map.get(state.sessions, session_id) do
          session -> send(caller, {:session_info, session_id, session})
          nil -> send(caller, {:error, :session_not_found})
        end

        loop(state)
    end
  end
end
