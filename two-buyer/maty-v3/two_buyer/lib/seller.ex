defmodule TwoBuyerMaty3.Seller do
  alias TwoBuyerMaty3.{MatyRole, SessionContext, Logger}

  @behaviour MatyRole
  @role :seller

  def setup() do
    send(self(), :install)
    :ok
  end

  @impl MatyRole
  def init_role(%SessionContext{} = session) do
    log("Initialising Session Context")
    log("Suspend with :title_handler")
    {:ok, %{session: session, current_handler: :title_handler}}
  end

  @impl MatyRole
  def handle_message(:install, %{current_handler: nil} = state) do
    log("Installing...")
    {:noreply, state}
  end

  def handle_message({:title, title, from_pid}, %{session: session} = state)
      when from_pid === session.buyer1 and state.current_handler === :title_handler do
    amount = lookup_price(title)
    log(:title_handler, "Received title=#{title}, sending quote=#{amount} to Buyer1")
    log(:title_handler, "Suspending with 'decision_handler'")

    send(session.buyer1, {:quote, amount, self()})
    {:suspend, :decision_handler, %{state | current_handler: :decision_handler}}
  end

  def handle_message({:address, addr, from_pid}, %{session: session} = state)
      when from_pid === session.buyer2 and state.current_handler === :decision_handler do
    date = shipping_date(addr)
    log(:decision_handler, "Received address=#{addr}, sending date=#{date} to Buyer2")

    send(session.buyer2, {:date, date, self()})
    {:stop, :normal, state}
  end

  def handle_message({:quit, :unit, from_pid}, %{session: session} = state)
      when from_pid === session.buyer2 and state.current_handler === :decision_handler do
    log(:decision_handler, "Received quit, stopping.")
    {:stop, :normal, state}
  end

  # fallback:
  def handle_message(msg, state) do
    log("Unexpected msg: #{inspect(msg)}, returning to message loop")
    {:noreply, state}
  end

  # -----------------------------------------------------------------
  defp lookup_price(_title_str), do: 155
  defp shipping_date(_addr), do: "2025-02-07"

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
