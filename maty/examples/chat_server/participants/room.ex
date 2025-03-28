defmodule ChatServer.Participants.Room do
  use Maty.Actor

  @role :room

  @impl
  @spec init_actor(pid()) :: {:ok, maty_actor_state()}
  def init_actor(ap_pid) do
    initial_state = %{callbacks: %{}, participants: %{}}

    register(
      ap_pid,
      @role,
      fn _, state -> {:suspend, {&__MODULE__.some_handler/4, :client}, state} end,
      initial_state
    )
  end
end
