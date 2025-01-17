defmodule Buyer1 do
  use GenServer

  @name __MODULE__

  def start_link() do
    GenServer.start_link(@name, %{}, name: @name)
  end

  def init_role(%SessionContext{} = session) do
    GenServer.cast(@name, {:init_role, session})
  end

  def send_title(title) do
    IO.puts("[Buyer1] Sending title=#{title} to Seller")
    send(session.seller, {:title, title})
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
  def handle_info({:quote, amount}, %{session: session} = state) do
    # We received "quote(int)" from Seller
    # Next: “Buyer1 -> Buyer2 : share(int)”
    IO.puts("[Buyer1] Received quote=#{amount}. Forwarding share to Buyer2...")

    buyer2 = session.buyer2
    share_amount = amount / 2
    send(buyer2, {:share, share_amount})
    {:noreply, state}
  end
end
