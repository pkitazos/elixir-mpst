defmodule TwoBuyerMaty3.Buyer2 do
  alias TwoBuyerMaty3.{MatyRole, SessionContext, Logger}

  @behaviour MatyRole
  @role :buyer1

  @impl MatyRole
  def init_role(%SessionContext{} = session) do
    log("Initialising Session Context")
    log("Suspending with 'share_handler'")
    {:ok, %{session: session, current_handler: :share_handler}}
  end

  @impl MatyRole

  def handle_message({:share, amount, from_pid}, %{session: session} = state)
      when from_pid === session.buyer1 and state.current_handler === :share_handler do
    log(:share_handler, "Received share=#{amount}")

    if amount > 100 do
      log(:share_handler, "share > 100, sending quit to Seller")

      send(session.seller, {:quit, :unit, self()})
      {:stop, :normal, state}
    else
      address = get_address()
      log(:share_handler, "share <= 100, sending address=#{address} to Seller")
      log(:share_handler, "Suspending with 'date_handler'")

      send(session.seller, {:address, address, self()})
      {:suspend, :date_handler, %{state | current_handler: :date_handler}}
    end

    {:stop, :normal, state}
  end

  def handle_message({:date, date, from_pid}, %{session: session} = state)
      when from_pid === session.seller and state.current_handler === :date_handler do
    log(:date_handler, "Received date=#{date}, finishing.")

    {:stop, :normal, state}
  end

  # fallback:
  def handle_message(msg, state) do
    log("Unexpected msg: #{inspect(msg)}, returning to message loop")
    {:noreply, state}
  end

  # -----------------------------------------------------------------
  defp get_address(), do: "18 Lilybank Gardens"

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
