defmodule ChatServer.Participants.Client do
  use Maty.Actor

  @role :client

  @impl
  @spec init_actor(pid()) :: {:ok, maty_actor_state()}
  def init_actor(ap_pid) do
    initial_state = %{callbacks: %{}, participants: %{}}

    register(
      ap_pid,
      @role,
      fn _, state -> {:suspend, {&__MODULE__.some_handler/4, :room}, state} end,
      initial_state
    )
  end
end
