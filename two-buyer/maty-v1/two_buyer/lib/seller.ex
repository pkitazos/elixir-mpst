defmodule TwoBuyerMaty1.Seller do
  use GenServer

  @name __MODULE__

  # Public API

  def start_link() do
    GenServer.start_link(@name, %{}, name: @name)
    # we name it __MODULE__ for convenience;
  end

  def init_role(%TwoBuyerMaty1.SessionContext{} = session) do
    # store the session context in the GenServer’s state
    GenServer.cast(@name, {:init_role, session})
  end

  # Internal GenServer callbacks

  @impl true
  def init(_initial_state) do
    {:ok, %{session: nil}}
  end

  @impl true
  def handle_cast({:init_role, session}, state) do
    {:noreply, %{state | session: session}}
  end

  @impl true
  def handle_info({:title, title}, %{session: session} = state) do
    # We received "title(str)" from Buyer1
    # Next: “Seller -> Buyer1 : quote(str)”
    IO.puts("[Seller] Got title=#{title}. Sending quote...")

    buyer1 = session.buyer1
    send(buyer1, {:quote, lookup_price(title)})
    {:noreply, state}
  end

  @impl true
  def handle_info({:address, addr}, %{session: session} = state) do
    # We received "address(addr)" from Buyer2
    # Next: "Seller -> Buyer2 : date(date)"
    IO.puts("[Seller] Got address=#{addr}. Sending date...")

    buyer2 = session.buyer2
    send(buyer2, {:date, shipping_date(addr)})
    {:noreply, state}
  end

  @impl true
  def handle_info({:quit, _}, state) do
    IO.puts("[Seller] Buyer2 decided to quit.")
    # in Maty, we do `return ()` or similar. In Elixir, we might just stop gracefully.
    {:stop, :normal, state}
  end

  # Utility placeholders
  defp lookup_price(title), do: String.length(title) * 5
  defp shipping_date(addr), do: Date.utc_today() |> Date.add(String.length(addr))
end
