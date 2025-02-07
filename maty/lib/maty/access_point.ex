defmodule Maty.AccessPoint do
  def start_link(roles) do
    initial_state = %{participants: Map.from_keys(roles, :queue.new())}

    pid = spawn_link(fn -> loop(initial_state) end)
    {:ok, pid}
  end

  defp loop(%{participants: participants} = state) do
    receive do
      {:register, role, pid, init_token} ->
        new_participants = Map.update(participants, role, &:queue.in({pid, init_token}, &1))

        if session_ready?(new_participants) do
          session_id = make_ref()
          {session_participants, updated_participants} = get_ready_participants(new_participants)

          session_participants
          |> Map.to_list()
          |> Enum.map(fn {pid, _role, init_token} ->
            send(pid, {:init_session, session_id, session_participants, init_token})
          end)

          loop(%{state | participants: updated_participants})
        else
          loop(%{state | participants: new_participants})
        end
    end
  end

  def session_ready?(participants) do
    not (participants
         |> Map.to_list()
         |> Enum.map(fn {_, q} -> :queue.is_empty(q) end)
         |> Enum.any?())
  end

  def get_ready_participants!(participants) do
    {ready_participants, role_queue_pairs} =
      participants
      |> Map.to_list()
      |> Enum.map(fn {role, q} ->
        {{:value, {pid, token}}, updated_queue} = :queue.out(q)
        {{pid, role, token}, {role, updated_queue}}
      end)
      |> Enum.unzip()

    {ready_participants, Enum.into(role_queue_pairs, %{})}
  end
end
