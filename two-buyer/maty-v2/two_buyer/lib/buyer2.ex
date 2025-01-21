defmodule TwoBuyerMaty2.Buyer2 do
  use GenServer
  alias TwoBuyerMaty2.SessionContext
  alias TwoBuyerMaty2.Logger

  @name __MODULE__
  @role :buyer2

  def start_link() do
    GenServer.start_link(@name, %{}, name: @name)
  end

  def init_role(%SessionContext{} = session) do
    GenServer.cast(@name, {:init_role, session})
  end

  # -----------------------------------------------------------------

  @impl true
  def init(_init_arg) do
    # we'll store two things in the state:
    #   1. session: references to the Buyer1, Seller PIDs
    #   2. current_handler: which "handler" is currently active
    {:ok, %{session: nil, current_handler: nil}}
  end

  @impl true
  def handle_cast({:init_role, session}, state) do
    log("Suspending with 'share_handler'")
    {:noreply, %{state | session: session, current_handler: :share_handler}}
  end

  @impl true
  def handle_info(msg, state) do
    # dispatch to whichever "handler" is active
    case state.current_handler do
      :share_handler -> handle_share(msg, state)
      :date_handler -> handle_date(msg, state)
    end
  end

  # -----------------------------------------------------------------

  defp handle_share({:share, amount, from_pid}, %{session: session} = state)
       when from_pid == session.buyer1 do
    # We received "share(int)" from Buyer1
    log(:share_handler, "Received share=#{amount}")

    # Next: we have a choice
    if amount > 100 do
      # Option 1: "Buyer2 -> Seller : quit(_)"
      log(:share_handler, "share > 100, sending quit to Seller")
      send(session.seller, {:quit, :unit, self()})
      # can either stop or remain alive
      {:stop, :normal, state}
    else
      # Option 2: "Buyer2 -> Seller : address(str)"
      address = get_address()
      log(:share_handler, "share <= 100, sending address=#{address} to Seller")
      log(:share_handler, "Suspending with 'date_handler'")
      send(session.seller, {:address, address, self()})
      # Next, we wait for date(date)
      {:noreply, %{state | current_handler: :date_handler}}
    end
  end

  defp handle_share({:share, _amount, from_pid}, state) do
    log(:share_handler, "Ignoring 'share' from pid=#{inspect(from_pid)}")
    {:noreply, state}
  end

  defp handle_share(_other_msg, state) do
    log(:share_handler, "Unexpected message")
    {:noreply, state}
  end

  defp handle_date({:date, date, from_pid}, %{session: session} = state)
       when from_pid == session.seller do
    # We received "date(date)" from Seller
    log(:date_handler, "Received date=#{date}, finishing.")
    # again, can either stop or remain alive
    {:stop, :normal, state}
  end

  defp handle_date({:date, _date, from_pid}, state) do
    log(:date_handler, "Ignoring 'date' from pid=#{inspect(from_pid)}")
    {:noreply, state}
  end

  defp handle_date(_other_msg, state) do
    log(:date_handler, "Unexpected message")
    {:noreply, state}
  end

  # -----------------------------------------------------------------
  # Utility stubs
  # -----------------------------------------------------------------
  defp get_address(), do: "18 Lilybank Gardens"

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
