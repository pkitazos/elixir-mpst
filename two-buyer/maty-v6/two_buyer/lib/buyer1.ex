defmodule MV6.Buyer1 do
  alias MV6.Logger
  use MV6.MatyActor

  @role :buyer1

  @impl true
  def init_actor({ap_pid, title}) do
    state = %{sessions: %{}, callbacks: %{}, ap_pid: ap_pid, role: @role}

    def send_title(session_id, state) do
      seller = get_in(state, [:sessions, session_id, :participants, :seller])

      maty_send(self(), seller, session_id, {:title, title})
      {:suspend, &__MODULE__.quote_handler/4}
    end

    updated_state =
      register(
        ap_pid,
        @role,
        &send_title/2,
        state
      )

    {:ok, updated_state}
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

  # -----------------------------------------------------------------

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
