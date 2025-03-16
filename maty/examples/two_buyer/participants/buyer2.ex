defmodule TwoBuyer.Participants.Buyer2 do
  # // alias Maty.Logger
  use Maty.Actor

  @role :buyer2

  # B2_a === Buyer1 & share(Int).Seller + {
  #   address(String). B2_b
  #   quit(Unit).end }
  @st {:share_handler,
       ["buyer1&share(float).{ seller!address(string).date_handler, seller!quit(unit).end }"]}
  # B2_b === Seller & date(Date).end
  @st {:date_handler, ["seller&date(date).end"]}

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

  @handler :share_handler
  def share_handler({:share, amount}, :buyer1, session, state) do
    # // log(:share_handler, "Received share=#{amount}")

    if amount > 100 do
      # // log(:share_handler, "share > 100, sending quit to Seller")

      maty_send(session, :seller, {:quit, :unit})
      {:done, :unit, state}
    else
      address = get_address()
      # // log(:share_handler, "share <= 100, sending address=#{address} to Seller")
      # // log(:share_handler, "Suspending with 'date_handler'")

      maty_send(session, :seller, {:address, address})
      {:suspend, {&__MODULE__.date_handler/4, :seller}, state}
    end
  end

  @handler :date_handler
  def date_handler({:date, _date}, :seller, _session, state) do
    # // log(:date_handler, "Received date=#{date}, finishing.")

    {:done, :unit, state}
  end

  # -----------------------------------------------------------------

  defp get_address(), do: "18 Lilybank Gardens"

  # -----------------------------------------------------------------

  # // defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
