defmodule MV6.Seller do
  alias MV6.Logger
  use MV6.MatyActor

  @role :seller

  @impl true
  def init_actor(ap_pid) do
    state = %{sessions: %{}, callbacks: %{}, ap_pid: ap_pid, role: @role}

    updated_state = register(ap_pid, @role, &__MODULE__.install/2, state)
    {:ok, updated_state}
  end

  # ------------------------------------------------------------------

  def install(session_id, %{ap_pid: ap_pid} = state) do
    register(
      ap_pid,
      @role,
      fn session_id, state ->
        install(session_id, state)
        {:suspend, &__MODULE__.title_handler/4, state}
      end,
      state
    )
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

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
