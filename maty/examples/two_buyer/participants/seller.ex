defmodule TwoBuyer.Participants.Seller do
  alias Maty.Logger
  use Maty.Actor

  @role :seller

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

  def title_handler({:title, title}, :buyer1, session, state) do
    amount = lookup_price(title)
    log(:title_handler, "Received title=#{title}, sending quote=#{amount} to Buyer1")
    log(:title_handler, "Suspending with 'decision_handler'")

    maty_send(session, :buyer1, {:quote, amount})
    {:suspend, {&__MODULE__.decision_handler/4, :buyer2}, state}
  end

  def decision_handler({:address, addr}, :buyer2, session, state) do
    date = shipping_date(addr)
    log(:decision_handler, "Received address=#{addr}, sending date=#{date} to Buyer2")

    maty_send(session, :buyer2, {:date, date})
    {:done, :unit, state}
  end

  def decision_handler({:quit, _}, :buyer2, _session, state) do
    {:done, :unit, state}
  end

  # -----------------------------------------------------------------

  defp lookup_price(_title_str), do: 150
  defp shipping_date(_addr_str), do: "2021-12-31"

  # -----------------------------------------------------------------

  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
