defmodule TwoBuyer.Participants.Buyer1 do
  use Maty.Actor

  @role :buyer1

  @st {:install, "+seller:{title(binary).quote_handler}"}
  @st {:quote_handler, "&seller:{quote(number).+buyer2:{share(number).end}}"}

  @impl true
  @spec on_link({pid(),  nil}, maty_actor_state()) :: {:ok, maty_actor_state()}
  # so this IS the local state just as with a regular handler
  # or perhaps we can give them access to both local and global state here
  # or now tha I think about it, forget the global state completely
  # just keep local session state
  def on_link({ap_pid, title}, initial_state) do
    MatyDSL.register(
      ap_pid,
      @role,
      [callback: :install, args: title],
      initial_state
    )
  end

  init_handler :install, title :: binary(), state do
    MatyDSL.send(:seller, {:title, title})
    MatyDSL.suspend(:quote_handler, state)
  end


  handler :quote_handler, :seller, {:quote, amount :: number()}, state do
    share_amount = amount / 2
    :quote_handler
    MatyDSL.send(:buyer2, {:share, share_amount})
    MatyDSL.done(state)
  end
end
