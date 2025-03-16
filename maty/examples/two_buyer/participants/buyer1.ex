defmodule TwoBuyer.Participants.Buyer1 do
  use Maty.Actor

  @role :buyer1

  # Seller + title(String). B1_b
  @st {:install, ["buyer1&title(string).quote_handler"]}

  # Seller & quote(String).Buyer2 + share(Int).end
  @st {:quote_handler, ["seller&quote(float).buyer2!share(float)"]}

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
  def install({:title, title}, :buyer1, session, state) do
    # if i change :buyer1 to @role I currently get an error
    maty_send(session, :seller, {:title, title})

    {:suspend, {&__MODULE__.quote_handler/4, :seller}, state}
  end

  # ------------------------------------------------------------------

  @handler :quote_handler
  def quote_handler({:quote, amount}, :seller, session, state) do
    share_amount = amount / 2

    # [re: ST] Let's say I want to typecheck this function
    # I can assume that this function has been annotated with a precondition ST (see handlerType)
    # then I use my haskell tc functions to check if this function based on its AST typechecks
    # if it does, we good and go on to the next function
    # I'm basically type-checking stuff like:
    # - am I permitted to send based on my curr session type
    # - check that my message type, message recipient, etc. all match my ST
    # - check that my function return type is okay (return type or suspend or whatever)
    maty_send(session, :buyer2, {:share, share_amount})

    # [re: ST]
    # a) am I done
    # b) if not am I in a point where I can suspend
    {:done, :unit, state}
  end
end
