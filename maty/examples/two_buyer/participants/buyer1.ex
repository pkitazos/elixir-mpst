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
        maty_send(state.sessions[session_id], :seller, {:title, title})
        {:suspend, {&__MODULE__.quote_handler/4, :seller}, state}
      end,
      initial_state
    )
  end

  # ------------------------------------------------------------------

  def quote_handler({:quote, amount}, :seller, session, state) do
    share_amount = amount / 2
    log(:quote_handler, "Received quote=#{amount}, sending share=#{share_amount} to Buyer2")
    log(:quote_handler, "Suspending with 'decision_handler'")

    # ! the maty_send function needs to know the actor's role in this interaction
    # if an actor only ever has one role in a session then this is trivially easy
    # if an actor can have multiple different roles in a session
    # then this is significantly harder to do from inside the message loop (if not impossible)
    maty_send(session, :buyer2, {:share, share_amount})

    {:done, :unit, state}
  end

  def quote_handler(_, _, _, state), do: {:continue, nil, state}

  # -----------------------------------------------------------------

  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
