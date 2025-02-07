defmodule Maty.AccessPoint do
  def start_link(session_context_module) do
    initial_state = %{
      sessions: %{},
      incomplete_session_ids: MapSet.new(),
      participant_init_tokens: %{},
      session_context_module: session_context_module
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
    receive do
      {:register, role, pid, init_token} ->
        maybe_session =
          incomplete_session_ids
          |> Enum.map(fn id -> {id, Map.fetch!(sessions, id)} end)
          |> Enum.find(fn {_session_id, map} -> Map.get(map, role) == nil end)

        case maybe_session do
          nil ->
            # * either no incomplete sessions
            # * or all incomplete sessions have a pid registered for the current role
            new_session_id = make_ref()

            new_session =
              struct!(state.session_context_module) |> Map.put(role, pid)

            updated_participant_init_tokens =
              Map.update(
                participant_init_tokens,
                pid,
                %{new_session_id => %{role => init_token}},
                &Map.put(&1, new_session_id, %{role => init_token})
              )

            loop(%{
              state
              | sessions: Map.put(sessions, new_session_id, new_session),
                incomplete_session_ids: MapSet.put(incomplete_session_ids, new_session_id),
                participant_init_tokens: updated_participant_init_tokens
            })

          {id, session} ->
            # * this is the first session that is missing this role
            updated_session = session |> Map.put(role, pid)

            updated_incomplete_session_ids =
              if is_incomplete?(updated_session) do
                incomplete_session_ids
              else
                # get the init_tokens of the other two participants
                # add this new one
                # send everything

                updated_session
                |> Map.from_struct()
                |> Map.to_list()
                |> Enum.map(fn {participant_role, pid} ->
                  token =
                    if participant_role != role do
                      get_in(state, [:participant_init_tokens, pid, id, participant_role])
                    else
                      init_token
                    end

                  send(pid, {:init_session, id, updated_session, participant_role, token, self()})
                end)

                MapSet.delete(incomplete_session_ids, id)
              end

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
              state
              | sessions: Map.put(sessions, id, updated_session),
                incomplete_session_ids: updated_incomplete_session_ids,
                participant_init_tokens: updated_participant_init_tokens
            })
        end
    end
  end

  # def inform_participants(session_id, session_participants) do
  # end

  defp is_incomplete?(session) do
    session |> Map.from_struct() |> Map.to_list() |> Enum.any?(fn {_, val} -> val == nil end)
  end
end
