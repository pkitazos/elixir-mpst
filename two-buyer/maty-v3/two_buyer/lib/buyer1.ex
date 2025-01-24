defmodule TwoBuyerMaty3.Buyer1 do
  alias TwoBuyerMaty3.{MatyRole, SessionContext, Logger}

  @behaviour MatyRole
  @role :buyer1

  def setup(data) do
    send(self, {:setup, data})
  end

  @impl MatyRole
  def init_role(%SessionContext{} = session) do
    log("Initialising Session Context")
    {:ok, %{session: session, current_handler: :quote_handler}}
  end

  @impl MatyRole

  # setup function can only be called by the role itself
  def handle_message({:setup, title, from_pid}, %{session: session} = state)
      when from_pid === session.buyer1 do
    log("Sending title=#{title} to Seller")
    log("Suspending with 'quote_handler'")

    send(pid, {:title, title, self()})
    {:noreply, state}
  end

  def handle_message({:quote, amount, from_pid}, %{session: session} = state)
      when from_pid === session.seller and state.current_handler === :quote_handler do
    share_amount = amount / 2
    log(:quote_handler, "Received quote=#{amount}, sending share=#{share_amount} to Buyer2")

    send(session.buyer2, {:share, share_amount, self()})
    {:stop, :normal, state}
  end

  # fallback:
  def handle_message(msg, state) do
    log("Unexpected msg: #{inspect(msg)}, returning to message loop")
    {:noreply, state}
  end

  # -----------------------------------------------------------------
  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
