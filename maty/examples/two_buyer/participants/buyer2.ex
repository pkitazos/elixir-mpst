defmodule TwoBuyer.Participants.Buyer2 do
  use Maty.Actor
  @after_compile Maty.Hook

  @role :buyer2

  # Buyer1 & share(Int).Seller + {
  #   address(String). B2_b
  #   quit(Unit).end }
  @st {:share_handler,
       "buyer1&{share(number).seller!{address(binary).date_handler, quit(unit).end}}}"}

  # Seller & date(Date).end
  @st {:date_handler, "seller&{date(date).end}"}

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
  @spec share_handler({:share, number()}, role(), session_ctx(), maty_actor_state()) ::
          suspend() | done()
  def share_handler({:share, amount}, :buyer1, session, state) do
    if amount > 100 do
      maty_send(session, :seller, {:quit, :unit})
      {:done, :unit, state}
    else
      address = get_address()

      maty_send(session, :seller, {:address, address})
      {:suspend, {&__MODULE__.date_handler/4, :seller}, state}
    end
  end

  @handler :date_handler
  @spec date_handler({:date, Date.t()}, role(), session_ctx(), maty_actor_state()) :: done()
  def date_handler({:date, _date}, :seller, _session, state) do
    {:done, :unit, state}
  end

  # -----------------------------------------------------------------

  @spec get_address() :: binary()
  defp get_address(), do: "18 " <> "Lilybank Gardens"
end
