defmodule TwoBuyer.Participants.Buyer1 do
  use Maty.Actor

  @role :buyer1

  @st {:install, "+seller:{title(binary).quote_handler}"}
  @st {:quote_handler, "&seller:{quote(number).+buyer2:{share(number).end}}"}

  @impl true
  @spec init_actor({pid(), binary()}) :: {:ok, maty_actor_state()}
  def init_actor({ap_pid, title}) do
    initial_state = %{sessions: %{}, callbacks: %{}}

    register(
      ap_pid,
      @role,
      MatyDSL.init_callback(:install, title),
      initial_state
    )
  end

  init_handler :install, {title :: binary()}, state do
    MatyDSL.send(:seller, {:title, title})
    MatyDSL.suspend(:quote_handler, state)
  end


  handler :quote_handler, :seller, {:quote, amount :: number()}, state do
    share_amount = amount / 2
    MatyDSL.send(:buyer2, {:share, share_amount})
    MatyDSL.done(state)
  end

end
