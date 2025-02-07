defmodule TwoBuyer.Participants.Buyer1 do
  alias Maty.Logger
  use Maty.Actor

  @role :buyer1

  @impl true
  def init_actor({ap_pid, title}) do
    initial_state = %{sessions: %{}, callbacks: %{}}

    register(
      ap_pid,
      @role,
      fn session_id, state ->
        participants = get_in(state, [:sessions, session_id, :participants])

        maty_send(participants.buyer1, participants.seller, session_id, {:title, title})
        {:suspend, &__MODULE__.quote_handler/4, state}
      end,
      initial_state
    )
  end

  # ------------------------------------------------------------------

  def quote_handler({:quote, amount}, from_pid, %{participants: participants} = session, state)
      when from_pid === participants.seller do
    share_amount = amount / 2
    log(:quote_handler, "Received quote=#{amount}, sending share=#{share_amount} to Buyer2")
    log(:quote_handler, "Suspending with 'decision_handler'")

    maty_send(
      participants.buyer1,
      participants.buyer2,
      session.id,
      {:share, share_amount}
    )

    {:done, :unit, state}
  end

  def quote_handler(_, _, _, state), do: {:continue, nil, state}

  # -----------------------------------------------------------------

  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
