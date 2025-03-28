defmodule ChatServer.Participants.Server do
  use Maty.Actor

  @role :server

  @impl
  @spec init_actor(pid()) :: {:ok, maty_actor_state()}
  def init_actor(ap_pid) do
    initial_state = %{callbacks: %{}, participants: %{}}

    register(ap_pid, @role, &install(&1, &2, ap_pid), initial_state)
  end

  @spec install(session_ctx(), maty_actor_state(), pid()) :: suspend()
  def install(_session, state, ap_pid) do
    {:ok, updated_state} =
      register(
        ap_pid,
        @role,
        &install(&1, &2, ap_pid),
        state
      )

    {:suspend, {&__MODULE__.some_handler/4, :client}, updated_state}
  end
end
