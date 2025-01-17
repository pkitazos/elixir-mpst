defmodule Buyer2 do
  use GenServer

  @name __MODULE__

  def start_link() do
    GenServer.start_link(@name, %{}, name: @name)
  end

  def init_role(%SessionContext{} = session) do
    GenServer.cast(@name, {:init_role, session})
  end

  @impl true
  def init(_args) do
    {:ok, %{session: nil}}
  end

  @impl true
  def handle_cast({:init_role, session}, state) do
    {:noreply, %{state | session: session}}
  end

  @impl true
  def handle_info({:share, amount}, %{session: session} = state) do
    # We received "share(int)" from Buyer1
    IO.puts("[Buyer2] Received share=#{amount}")

    # Next: we have a choice
    if amount > 100 do
      # Option 1: "Buyer2 -> Seller : quit(_)"
      IO.puts("[Buyer2] share > 100, sending quit(...)")
      send(session.seller, {:quit, :unit})
      # can either stop or remain alive

      {:noreply, state}
    else
      # Option 2: "Buyer2 -> Seller : address(str)"
      IO.puts("[Buyer2] share <= 100, sending address(...)")
      send(session.seller, {:address, get_address()})
      # Next, we wait for date(date)
      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:date, date}, state) do
    # We received "date(date)" from Seller
    IO.puts("[Buyer2] Received date=#{date}, finishing.")
    # again, can either stop or remain alive
    {:stop, :normal, state}
  end

  # Utility placeholders
  defp get_address(), do: "18 Lilybank Gardens"
end
