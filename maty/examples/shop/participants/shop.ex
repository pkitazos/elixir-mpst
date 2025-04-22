defmodule Shop.Participants.Shop do
  use Maty.Actor

  @role :shop

  @impl true
  @spec on_link({pid(), pid(), map()}) :: {:ok, maty_actor_state()}
  def on_link({customer_ap, staff_ap, initial_stock}, initial_state) do
    state = MatyDSL.State.set(initial_state, initial_stock)

    MatyDSL.register(
      customer_ap,
      @role,
      [callback: :install, args: customer_ap],
      state
    )

    MatyDSL.register(
      staff_ap,
      @role,
      [callback: :install, args: staff_ap],
      state
    )
  end

  init_handler :install, ap_pid :: pid(), state do
    {:ok, updated_state} =
      MatyDSL.register(
        ap_pid,
        @role,
        [callback: :install, args: ap_pid],
        state
      )

    MatyDSL.suspend(:item_req_handler, updated_state)
  end

  handler :item_req_handler, :customer, {:request_items, nil}, state do
    items = MatyDSL.State.get(state)
    summary = summarise_items(items)

    MatyDSL.send(:customer, {:item_summary, summary})
    MatyDSL.suspend(:cust_req_handler, state)
  end

  handler :cust_req_handler, :customer, {:get_item_info, item_id}, state do
    items = MatyDSL.State.get(state)
    item_info = lookup_item(items, item_id)

    MatyDSL.send(:customer, {:item_info, item_info})
    MatyDSL.suspend(:cust_req_handler, state)
  end

  handler :cust_req_handler, :customer, {:checkout, {item_ids, details}}, state do
    items = MatyDSL.State.get(state)
    in_stock = check_capacity(items, item_ids)

    if in_stock do
      MatyDSL.send(:customer, {:payment_processing, nil})
      new_items = decrease_stock(items, item_ids)
      updated_state = MatyDSL.State.set(new_items)
      total = calculate_cost(items, item_ids)

      MatyDSL.send(:payment_processor, {:buy, {total, details}})
      MatyDSL.suspend(:payment_handler, updated_state)
    else
      MatyDSL.send(:customer, {:out_of_stock, nil})
      MatyDSL.suspend(:cust_req_handler, state)
    end
  end

  @spec summarise_items(map()) :: binary()
  def summarise_items(_items) do
    "something interesting"
  end

  @spec lookup_item(map(), binary()) :: binary()
  def lookup_item(_items, _item_id) do
    "something interesting"
  end

  @spec check_capacity(map(), binary()) :: binary()
  def check_capacity(_items, _item_id) do
    "something interesting"
  end

  @spec decrease_stock(map(), binary()) :: binary()
  def decrease_stock(_items, _item_id) do
    "something interesting"
  end

  @spec calculate_cost(map(), binary()) :: binary()
  def calculate_cost(_items, _item_id) do
    "something interesting"
  end
end
