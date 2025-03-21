defmodule TwoBuyer.Participants.Seller do
  use Maty.Actor

  @role :seller

  # Buyer1 & title(String).Buyer1 + quote(Int). S_b
  @st {:title_handler, ["buyer1&title(string).buyer1!quote(float).decision_handler"]}

  # Buyer2 &{
  #   address(String).Buyer2 + date(Date).end
  #   quit(Unit).end
  @st {:decision_handler,
       ["buyer2&address(string).buyer2!date(date).end", "buyer2&quit(unit).end"]}

  @impl true
  def init_actor(ap_pid) do
    initial_state = %{sessions: %{}, callbacks: %{}, global: %{ap_pid: ap_pid}}

    {:ok, updated_state} =
      register(
        ap_pid,
        @role,
        &__MODULE__.install/2,
        initial_state
      )

    {:ok, updated_state}
  end

  # ------------------------------------------------------------------

  def install(_session, state) do
    {:ok, updated_state} =
      register(
        state.global.ap_pid,
        @role,
        &__MODULE__.install/2,
        state
      )

    {:suspend, {&__MODULE__.title_handler/4, :buyer1}, updated_state}
  end

  @handler :title_handler
  def title_handler({:title, title}, :buyer1, session, state) do
    amount = lookup_price(title)

    maty_send(session, :buyer1, {:quote, amount})
    {:suspend, {&__MODULE__.decision_handler/4, :buyer2}, state}
  end

  @handler :decision_handler
  def decision_handler({:address, addr}, :buyer2, session, state) do
    date = shipping_date(addr)

    maty_send(session, :buyer2, {:date, date})
    {:done, :unit, state}
  end

  @handler :decision_handler
  def decision_handler({:quit, _}, :buyer2, _session, state), do: {:done, :unit, state}

  # -----------------------------------------------------------------

  defp lookup_price(_title_str), do: 150
  defp shipping_date(_addr_str), do: "2021-12-31"
end
