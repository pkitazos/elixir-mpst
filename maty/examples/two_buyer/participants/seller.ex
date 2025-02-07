defmodule TwoBuyer.Participants.Seller do
  alias Maty.Logger
  use Maty.Actor

  @role :seller

  @impl true
  def init_actor(ap_pid) do
    initial_state = %{sessions: %{}, callbacks: %{}, ap_pid: ap_pid, role: @role}

    register(ap_pid, @role, &__MODULE__.install/2, initial_state)
  end

  # ------------------------------------------------------------------

  def install(_session_id, %{ap_pid: ap_pid} = state) do
    register(
      ap_pid,
      @role,
      &__MODULE__.install/2,
      state
    )

    {:suspend, &__MODULE__.title_handler/4, state}
  end

  def title_handler({:title, title}, from_pid, %{participants: participants} = session, state)
      when from_pid === participants.buyer1 do
    amount = lookup_price(title)
    log(:title_handler, "Received title=#{title}, sending quote=#{amount} to Buyer1")
    log(:title_handler, "Suspending with 'decision_handler'")

    maty_send(
      participants.seller,
      participants.buyer1,
      session.id,
      {:quote, amount}
    )

    {:suspend, &decision_handler/4, state}
  end

  def title_handler(_, _, _, state), do: {:continue, nil, state}

  def decision_handler({:address, addr}, from_pid, %{participants: participants} = session, state)
      when from_pid === participants.buyer2 do
    date = shipping_date(addr)
    log(:decision_handler, "Received address=#{addr}, sending date=#{date} to Buyer2")

    maty_send(
      participants.seller,
      participants.buyer2,
      session.id,
      {:date, date}
    )

    {:done, :unit, state}
  end

  def decision_handler({:quit, _}, from_pid, %{participants: participants}, state)
      when from_pid === participants.buyer2 do
    {:done, :unit, state}
  end

  def decision_handler(_, _, _, state), do: {:continue, nil, state}

  # -----------------------------------------------------------------

  defp lookup_price(_title_str), do: 150
  defp shipping_date(_addr_str), do: "2021-12-31"

  # -----------------------------------------------------------------

  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
