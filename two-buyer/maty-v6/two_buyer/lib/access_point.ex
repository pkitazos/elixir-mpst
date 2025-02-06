defmodule MV6.AccessPoint do
  alias MV6.{SessionContext}

  @type role :: atom()
  @type session_id :: reference()
  @type init_token :: reference()

  @type registered_participants :: %{role() => pid() | nil}
  @type incomplete_session_ids :: %MapSet.t(){session_id()}

  @type access_point_state :: %{
          sessions: %{session_id() => registered_participants()},
          incomplete_session_ids: incomplete_session_ids(),
          participant_init_tokens: %{pid() => %{session_id() => %{role() => init_token()}}}
        }

  # ------------------------------------------------------------------

  def start_link() do
    initial_state = %{
      sessions: %{},
      incomplete_session_ids: MapSet.new(),
      participant_init_tokens: %{}
    }

    pid = spawn_link(fn -> loop(initial_state) end)
    {:ok, pid}
  end

  defp loop(
         %{
           sessions: sessions,
           incomplete_session_ids: incomplete_session_ids,
           participant_init_tokens: participant_init_tokens
         } = state
       ) do
    ap = self()

    receive do
      {:register, role, pid, init_token} ->
        maybe_session =
          incomplete_session_ids
          |> Enum.map(fn id -> {id, Map.fetch!(sessions, id)} end)
          |> Enum.find(fn {session_id, map} -> Map.get(map, my_role) == nil end)

        case maybe_session do
          nil ->
            # * either no incomplete sessions
            # * or all incomplete sessions have a pid registered for the current role
            new_session_id = make_ref()
            new_session = %SessionContext{} |> update_session(role, pid)

            # since this is a new session it is definitely not full, can just add it to sessions
            # must also add this id to our new incomplete sessions list

            # need to also add this participant to our participants map

            # a participant could take part in a session as multiple roles (hence the map)
            # so when we register a participant it is safe to say that they were not previously already registered under this role
            # certainly not in this branch of the case statement
            # in fact we know that this is a new session and therefore there are no other roles this participant is registered under
            # so we can use Map.put
            updated_participant_init_tokens =
              Map.update(
                participant_init_tokens,
                pid,
                %{new_session_id => %{role => init_token}},
                &Map.put(&1, new_session_id, %{role => init_token})
              )

            loop(%{
              sessions: Map.put(sessions, new_session_id, new_session),
              incomplete_session_ids: MapSet.put(incomplete_session_ids, new_session),
              participant_init_tokens: updated_participant_init_tokens
            })

          {id, session} ->
            # * this is the first session that is missing this role
            updated_session = session |> update_session(role, pid)

            updated_incomplete_session_ids =
              if is_incomplete?(updated_session) do
                MapSet.delete(incomplete_session_ids, id)
                send(ap, {:session_ready, id, ap})
              else
                incomplete_session_ids
              end

            # while the session definitely existed before, there's no guarantee that the participant has registered with the ap before

            updated_participant_init_tokens =
              Map.update(
                participant_init_tokens,
                pid,
                %{id => %{role => init_token}},
                fn participant_sessions ->
                  Map.update(
                    participant_sessions,
                    id,
                    %{role => init_token},
                    &Map.put(&1, role, init_token)
                  )
                end
              )

            loop(%{
              sessions: Map.put(sessions, id, updated_session),
              incomplete_session_ids: updated_incomplete_session_ids,
              participant_init_tokens: updated_participant_init_tokens
            })
        end

      {:session_ready, session_id, ^ap} ->
        participants = Map.fetch!(sessions, id)

        participants
        |> Map.to_list()
        |> Enum.map(fn {role, pid} ->
          init_token = get_in(participant_init_tokens, [pid, session_id, role])
          send(pid, {:init_session, session_id, participants, role, init_token, ap})
        end)
    end
  end

  defp update_session(session, role, pid), do: %SessionContext{session | {role, pid}}

  defp is_incomplete?(session) do
    session |> Map.from_struct() |> Map.to_list() |> Enum.any?(fn {_, val} -> val == nil end)
  end
end
