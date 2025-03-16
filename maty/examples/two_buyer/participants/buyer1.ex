defmodule TwoBuyer.Participants.Buyer1 do
  # // alias Maty.Logger
  use Maty.Actor

  @role :buyer1

  # B1_a === Seller + title(String). B1_b
  @st {:install, ["buyer1&title(string).quote_handler"]}
  # B1_b === Seller & quote(String).Buyer2 + share(Int).end
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
  def install({:title, title}, @role, session, state) do
    maty_send(session, :seller, {:title, title})

    {:suspend, {&__MODULE__.quote_handler/4, :seller}, state}
  end

  # ------------------------------------------------------------------

  @handler :quote_handler
  def quote_handler({:quote, amount}, :seller, session, state) do
    share_amount = amount / 2
    # // log(:quote_handler, "Received quote=#{amount}, sending share=#{share_amount} to Buyer2")
    # // log(:quote_handler, "Suspending with 'decision_handler'")

    # [re: ST] Let's say I want to typecheck this function
    # I can assume that this function has been annotated with a precondition ST (see handlerType)
    # then I use my haskell tc functions to check if this funciton based on its AST typechecks
    # if it does, we good and go on to the next funciton
    # I'm basically type-checking stuff like:
    # - am I permitted to send based on my curr session type
    # - check that my message type, message recipient, etc. all match my ST
    # - check that my funciton return type is okay (return type or suspend or whatever)
    maty_send(session, :buyer2, {:share, share_amount})

    # [re: ST]
    # a) am I done
    # b) if not am I in a point where I can suspend
    {:done, :unit, state}
  end

  # -----------------------------------------------------------------

  # // defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
