defmodule Seller do
  use GenServer

  @name __MODULE__

  defstruct [
    :current_title,
    :current_quote,
    :delivery_date,
    buyer1: nil,
    buyer2: nil
  ]

  # Client API

  def start_link do
    GenServer.start_link(@name, %Seller{}, name: @name)
  end

  def receive_title(title, from_buyer) do
    GenServer.cast(@name, {:title, title, from_buyer})
  end

  def send_quote(buyer1, buyer2) do
    GenServer.cast(@name, {:send_quotes, buyer1, buyer2})
  end

  def receive_decision(decision, buyer2, address \\ nil) do
    GenServer.cast(@name, {:buyer_decision, decision, buyer2, address})
  end

  # Server Callbacks

  @impl true
  def init(state) do
    {:ok, state}
  end

  @impl true
  def handle_cast({:title, title, buyer1}, state) do
    # when receiving a title, store it and the buyer1 reference
    new_state = %{
      state
      | current_title: title,
        buyer1: buyer1,
        current_quote: calculate_quote(title)
    }

    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:send_quotes, buyer1, buyer2}, %{current_quote: quote} = state)
      when not is_nil(quote) do
    # send quote to both buyers
    GenServer.cast(buyer1, {:receive_quote, quote})
    GenServer.cast(buyer2, {:receive_quote, quote})

    # store buyer2 reference
    new_state = %{state | buyer2: buyer2}
    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:buyer_decision, :ok, buyer2, address}, state) do
    # generate delivery date
    delivery_date = Date.add(Date.utc_today(), 14)

    GenServer.cast(buyer2, {:delivery_date, delivery_date})

    new_state = %{state | delivery_date: delivery_date}
    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:buyer_decision, :quit, _buyer2, _address}, state) do
    {:noreply, %Seller{}}
  end

  # Helper functions

  defp calculate_quote(title) do
    # in a real application, this would have actual business logic
    # for this example I'll just the price to 100 * the length of the title
    String.length(title) * 100
  end
end
