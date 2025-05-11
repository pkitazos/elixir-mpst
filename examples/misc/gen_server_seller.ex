defmodule TwoBuyer.Seller.GenServerImpl do
  use GenServer
  require Logger

  def start_link(_opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @impl true
  def init(_initial_args) do
    initial_state = %{b1_pid: nil, b2_pid: nil}
    {:ok, initial_state}
  end

  @impl true
  def handle_cast({:title, book_title, buyer1_pid}, state) do
    amount = lookup_price(book_title)
    GenServer.cast(buyer1_pid, {:quote, amount})
    {:noreply, %{state | b1_pid: buyer1_pid}}
  end

  @impl true
  def handle_cast({:address, delivery_address, buyer2_pid}, state) do
    delivery_date = shipping_date(delivery_address)
    GenServer.cast(buyer2_pid, {:delivery_date_info, delivery_date})
    {:noreply, %{state | b2_pid: buyer2_pid}}
  end

  @impl true
  def handle_cast({:quit, buyer2_pid}, state) do
    {:noreply, %{state | b2_pid: buyer2_pid}}
  end

  @spec lookup_price(binary()) :: number()
  defp lookup_price(_title), do: 150

  @spec shipping_date(binary()) :: Date.t()
  defp shipping_date(_addr_str), do: ~D[2021-12-31]
end
