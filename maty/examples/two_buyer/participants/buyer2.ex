defmodule TwoBuyer.Participants.Buyer2 do
  alias Maty.Logger
  use Maty.Actor

  @role :buyer2

  @impl true
  def init_actor(ap_pid) do
    initial_state = %{sessions: %{}, callbacks: %{}}

    register(
      ap_pid,
      @role,
      fn _, state -> {:suspend, {&__MODULE__.share_handler/4, :buyer1}, state} end,
      initial_state
    )
  end

  def share_handler({:share, amount}, :buyer1, {session, role_me}, state) do
    log(:share_handler, "Received share=#{amount}")

    if amount > 100 do
      log(:share_handler, "share > 100, sending quit to Seller")

      maty_send({session, role_me}, :seller, {:quit, :unit})
      {:done, :unit, state}
    else
      address = get_address()
      log(:share_handler, "share <= 100, sending address=#{address} to Seller")
      log(:share_handler, "Suspending with 'date_handler'")

      maty_send({session, role_me}, :seller, {:address, address})
      {:suspend, {&__MODULE__.date_handler/4, :seller}, state}
    end
  end

  def date_handler({:date, date}, :seller, _session, state) do
    log(:date_handler, "Received date=#{date}, finishing.")

    {:done, :unit, state}
  end

  # -----------------------------------------------------------------

  defp get_address(), do: "18 Lilybank Gardens"

  # -----------------------------------------------------------------

  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
