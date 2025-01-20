defmodule Buyer do
  use GenServer

  defstruct [
    # :buyer1 or :buyer2
    :role,
    # String, only needed for buyer1
    :title,
    # Integer
    :quote,
    # Integer, only needed for buyer2
    :proposed_split,
    # Date, only needed for buyer2
    :delivery_date,
    # PID of the seller
    :seller
  ]

  # Client API

  def start_link(role, seller) do
    GenServer.start_link(__MODULE__, %Buyer{role: role, seller: seller}, name: via_tuple(role))
  end

  def request_title(buyer1, title) do
    GenServer.cast(via_tuple(:buyer1), {:request_title, title})
  end

  def propose_split(buyer1, buyer2, amount) do
    GenServer.cast(via_tuple(:buyer1), {:propose_split, amount, buyer2})
  end

  def accept_quote(buyer2, address) do
    GenServer.cast(via_tuple(:buyer2), {:accept, address})
  end

  def reject_quote(buyer2) do
    GenServer.cast(via_tuple(:buyer2), {:reject})
  end

  # Server Callbacks

  @impl true
  def init(state) do
    {:ok, state}
  end

  # Buyer1 handlers

  @impl true
  def handle_cast({:request_title, title}, %{role: :buyer1, seller: seller} = state) do
    # send title to seller
    Seller.receive_title(title, self())
    {:noreply, %{state | title: title}}
  end

  @impl true
  def handle_cast({:receive_quote, quote}, %{role: :buyer1} = state) do
    # Buyer1 receives quote from seller
    {:noreply, %{state | quote: quote}}
  end

  @impl true
  def handle_cast({:propose_split, amount, buyer2}, %{role: :buyer1} = state) do
    # Send half quote proposal to Buyer2
    GenServer.cast(buyer2, {:receive_split_proposal, amount})
    {:noreply, state}
  end

  # Buyer2 handlers

  @impl true
  def handle_cast({:receive_quote, quote}, %{role: :buyer2} = state) do
    # Buyer2 receives quote from seller
    {:noreply, %{state | quote: quote}}
  end

  @impl true
  def handle_cast({:receive_split_proposal, amount}, %{role: :buyer2} = state) do
    # Buyer2 receives split proposal from Buyer1
    {:noreply, %{state | proposed_split: amount}}
  end

  @impl true
  def handle_cast({:accept, address}, %{role: :buyer2, seller: seller} = state) do
    # Buyer2 accepts and sends address
    Seller.receive_decision(:ok, self(), address)
    {:noreply, state}
  end

  @impl true
  def handle_cast({:reject}, %{role: :buyer2, seller: seller} = state) do
    # Buyer2 rejects
    Seller.receive_decision(:quit, self())
    {:noreply, state}
  end

  @impl true
  def handle_cast({:delivery_date, date}, %{role: :buyer2} = state) do
    # Buyer2 receives delivery date from seller
    {:noreply, %{state | delivery_date: date}}
  end

  # Helper functions

  defp via_tuple(role) do
    {:via, Registry, {BuyerRegistry, role}}
  end
end
