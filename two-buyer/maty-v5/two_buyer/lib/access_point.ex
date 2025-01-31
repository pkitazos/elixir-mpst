defmodule TwoBuyerMaty5.AccessPoint do
  alias TwoBuyerMaty5.{SessionContext}

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

    receive do
      {:ok, session_id} -> {:ok, session_id}
    end
  end

  def register(ap_pid, session_id, role, caller, main) do
    send(ap_pid, {:register, session_id, role, caller, main})
    :ok
  end

  def fetch_session(ap_pid, session_id, caller, main) do
    send(ap_pid, {:fetch_session, session_id, caller, main})
  end

  defp loop(%{sessions: sessions, next_session_id: next_session_id} = state) do
    receive do
      # in our case the requester would only ever be the seller (maybe)
      {:create_session, caller} ->
        session_id = "session_#{next_session_id}"
        new_sessions = Map.put(sessions, session_id, %SessionContext{})

        send(caller, {:ok, session_id})
        loop(%{state | sessions: new_sessions, next_session_id: next_session_id + 1})

      {:register, session_id, role, caller, main} ->
        updated_session =
          sessions
          |> maybe_init_session(session_id)
          |> update_session(role, caller)

        updated_sessions = Map.put(sessions, session_id, updated_session)

        send(main, {:ok, session_id})
        loop(%{state | sessions: updated_sessions})

      {:fetch_session, session_id, caller, main} ->
        # need to keep track of who requested the session
        # once everyone has registered, we can send the session info back automatically?
        # or we only send this info back when requested
        # what we could then keep track of is who requested the session
        # and then when everyone has requested the session, we can send the session info back to the main process?

        # session_info = Map.get(sessions, session_id)
        # IO.puts("[DEBUG] session_info: #{inspect(session_info)}")
        # caller_role =
        #   session_info
        #   |> Map.from_struct()
        #   |> Enum.find(fn {_key, val} -> val == caller end)
        #   |> elem(0)

        # IO.puts("[DEBUG] session #{session_id} requested by caller_role: #{caller_role}")

        case Map.get(sessions, session_id) do
          nil ->
            send(caller, {:error, :session_not_found})
            send(main, {:error, :session_not_found})

          session ->
            participants = Map.take(session, [:seller, :buyer1, :buyer2])
            send(caller, {:session_participants, session_id, participants, self()})
            send(main, {:ok, session_id})
        end

        loop(state)
    end
  end

  defp maybe_init_session(sessions_map, session_id) do
    case Map.get(sessions_map, session_id) do
      nil -> %SessionContext{}
      session -> session
    end
  end

  defp update_session(session, :seller, pid), do: %SessionContext{session | seller: pid}
  defp update_session(session, :buyer1, pid), do: %SessionContext{session | buyer1: pid}
  defp update_session(session, :buyer2, pid), do: %SessionContext{session | buyer2: pid}
end
