defmodule TwoBuyer.Participants.Seller do
  use Maty.Actor

  @role :seller

  @st {:title_handler, "&buyer1:{title(binary).+buyer1:{quote(number).decision_handler}}"}

  @st {:decision_handler, "&buyer2:{address(binary).+buyer2:{date(date).end, quit(unit).end}}"}

  @impl true
  @spec init_actor(pid()) :: {:ok, maty_actor_state()}
  def init_actor(ap_pid) do
    initial_state = %{sessions: %{}, callbacks: %{}}

    {:ok, updated_state} =
      register(
        ap_pid,
        @role,
        &install(&1, &2, ap_pid),
        initial_state
      )

    {:ok, updated_state}
  end

  @spec install(session(), maty_actor_state(), pid()) :: suspend()
  def install(_session, state, ap_pid) do
    {:ok, updated_state} =
      register(
        ap_pid,
        @role,
        &install(&1, &2, ap_pid),
        state
      )

    {:suspend, {&__MODULE__.title_handler/4, :buyer1}, updated_state}
  end

  @handler :title_handler
  @spec title_handler({:title, binary()}, role(), session_ctx(), maty_actor_state()) :: suspend()
  def title_handler({:title, title}, :buyer1, session, state) do
    amount = lookup_price(title)

    maty_send(session, :buyer1, {:quote, amount})
    {:suspend, {&__MODULE__.decision_handler/4, :buyer2}, state}
  end

  @handler :decision_handler
  @spec decision_handler({:address, binary()}, role(), session_ctx(), maty_actor_state()) ::
          done()
  def decision_handler({:address, addr}, :buyer2, session, state) do
    date = shipping_date(addr)

    maty_send(session, :buyer2, {:date, date})
    {:done, :unit, state}
  end

  @handler :decision_handler
  @spec decision_handler({:quit, :unit}, role(), session_ctx(), maty_actor_state()) :: done()
  def decision_handler({:quit, :unit}, :buyer2, _session, state), do: {:done, :unit, state}

  @spec lookup_price(binary()) :: number()
  defp lookup_price(_title_str), do: 150

  @spec shipping_date(binary()) :: Date.t()
  defp shipping_date(_addr_str), do: ~D[2021-12-31]
end
