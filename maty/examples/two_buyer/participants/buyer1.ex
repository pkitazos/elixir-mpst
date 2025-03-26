defmodule TwoBuyer.Participants.Buyer1 do
  use Maty.Actor
  # @after_compile Maty.Hook

  @role :buyer1

  # Seller + title(String). B1_b
  @st {:install, "buyer1&{title(binary).quote_handler}"}

  # Seller & quote(String).Buyer2 + share(Int).end
  @st {:quote_handler, "seller&{quote(number).buyer2!{share(number)}}"}

  @impl true
  def init_actor({ap_pid, title}) do
    initial_state = %{sessions: %{}, callbacks: %{}}

    register(
      ap_pid,
      @role,
      fn session, state ->
        install({:title, title}, @role, session, state)
      end,
      initial_state
    )
  end

  @handler :install
  @spec install({:title, binary()}, role(), session_ctx(), maty_actor_state()) :: suspend()
  def install({:title, title}, @role, session, state) do
    maty_send(session, :seller, {:title, title})
    {:suspend, {&__MODULE__.quote_handler/4, :seller}, state}
  end

  # ------------------------------------------------------------------

  @handler :quote_handler
  @spec quote_handler({:quote, number()}, role(), session_ctx(), maty_actor_state()) :: done()
  def quote_handler({:quote, amount}, :seller, session, state) do
    share_amount = amount / 2

    maty_send(session, :buyer2, {:share, share_amount})
    {:done, :unit, state}
  end
end
