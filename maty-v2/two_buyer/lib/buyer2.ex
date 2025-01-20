defmodule TwoBuyerMaty2.Buyer2 do
  use GenServer

  @name __MODULE__

  def start_link() do
    GenServer.start_link(@name, %{}, name: @name)
  end

  def init_role(%TwoBuyerMaty2.SessionContext{} = session) do
    GenServer.cast(@name, {:init_role, session})
  end

  # -----------------------------------------------------------------

  @impl true
  def init(_init_arg) do
    # we'll store two things in the state:
    #   1. session: references to the Buyer1, Seller PIDs, etc.
    #   2. current_handler: which "handler" is currently active
    {:ok, %{session: nil, current_handler: nil}}
  end

  @impl true
  def handle_cast({:init_role, session}, state) do
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

  defp handle_share({:share, amount}, %{session: session} = state) do
    # We received "share(int)" from Buyer1
    IO.puts("[Buyer2] Received share=#{amount}")

    # Next: we have a choice
    if amount > 100 do
      # Option 1: "Buyer2 -> Seller : quit(_)"
      IO.puts("[Buyer2] share > 100, sending quit(...)")
      send(session.seller, {:quit, :unit})
      # can either stop or remain alive
      {:stop, :normal, state}
    else
      # Option 2: "Buyer2 -> Seller : address(str)"
      IO.puts("[Buyer2] share <= 100, sending address(...)")
      send(session.seller, {:address, get_address()})
      # Next, we wait for date(date)
      {:noreply, %{state | current_handler: :date_handler}}
    end
  end

  defp handle_share(_other_msg, state) do
    IO.puts("[Buyer2] (share_handler) got unexpected message")
    {:noreply, state}
  end

  defp handle_date({:date, date}, state) do
    # We received "date(date)" from Seller
    IO.puts("[Buyer2] Received date=#{date}, finishing.")
    # again, can either stop or remain alive
    {:stop, :normal, state}
  end

  defp handle_date(_other_msg, state) do
    IO.puts("[Buyer2] (date_handler) got unexpected message")
    {:noreply, state}
  end

  # -----------------------------------------------------------------
  # Utility stubs
  # -----------------------------------------------------------------
  defp get_address(), do: "18 Lilybank Gardens"
end
