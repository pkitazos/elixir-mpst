defmodule Shop.Participants.Shop do
  use Maty.Actor

  @role :shop

  @st {:install_cust, "item_req_handler"}

  @st {:item_req_handler, "&customer:{\
                                request_items(nil).+customer:{\
                                      items(string[]).cust_req_handler\
                                }\
                          }"}

  @st {:cust_req_handler, "&customer:{\
                                checkout((string[], string)).+customer:{\
                                      processing_payment(nil).+payment_processor:{\
                                            buy((string, number)).payment_handler\
                                      },\
                                      out_of_stock(nil).end\
                                }\
                          }"}

  @st {:payment_handler, "&payment_processor:{\
                              ok(nil).+customer:{\
                                    ok(nil).end\
                              },\
                              declined(nil).+customer:{\
                                    declined(nil).end\
                              }\
                          }"}

  @st {:install_staff, "staff_admin_handler"}
  @st {:staff_admin_handler, "&staff:{\
                                add_item((string,string,number,number)).staff_admin_handler,\
                                remove_item(string).staff_admin_handler\
                              }"}

  @impl true
  @spec on_link({pid(), pid(), map()}, maty_actor_state()) :: {:ok, maty_actor_state()}
  def on_link({customer_ap, staff_ap, initial_stock}, initial_state) do
    MatyDSL.register(
      customer_ap,
      @role,
      [callback: :install_cust, args: [customer_ap, initial_stock]],
      initial_state
    )

    MatyDSL.register(
      staff_ap,
      @role,
      [callback: :install_staff, args: [initial_stock]],
      initial_state
    )
  end

  init_handler :install_cust, {ap_pid :: pid(), initial_stock :: map()}, state do
    intermediate_state = Maty.DSL.State.set(state, initial_stock)

    {:ok, updated_state} =
      MatyDSL.register(
        ap_pid,
        @role,
        [callback: :install_cust, args: [ap_pid]],
        intermediate_state
      )

    MatyDSL.suspend(:item_req_handler, updated_state)
  end

  init_handler :install_staff, initial_stock :: map(), state do
    updated_state = Maty.DSL.State.set(state, initial_stock)
    MatyDSL.suspend(:staff_admin_handler, updated_state)
  end

  handler :item_req_handler, :customer, {:request_items, nil}, state do
    items = Maty.DSL.State.get(state)
    summary = summarise_items(items)

    MatyDSL.send(:customer, {:items, summary})
    MatyDSL.suspend(:cust_req_handler, state)
  end

  handler :cust_req_handler,
          :customer,
          {:checkout, {item_ids, details} :: {list(binary()), binary()}},
          state do
    items = Maty.DSL.State.get(state)
    item_id = "item 1"
    in_stock = check_capacity(items, item_id)

    if in_stock do
      MatyDSL.send(:customer, {:processing_payment, nil})
      new_items = decrease_stock(items, item_id)
      updated_state = Maty.DSL.State.set(state, new_items)
      total = calculate_cost(items, item_ids)

      MatyDSL.send(:payment_processor, {:buy, {details, total}})
      MatyDSL.suspend(:payment_handler, updated_state)
    else
      MatyDSL.send(:customer, {:out_of_stock, nil})
      MatyDSL.done(state)
    end
  end

  handler :payment_handler, :payment_processor, {:ok, nil}, state do
    MatyDSL.send(:customer, {:ok, nil})
    MatyDSL.suspend(:cust_req_handler, state)
  end

  handler :payment_handler, :payment_processor, {:declined, nil}, state do
    MatyDSL.send(:customer, {:declined, nil})
    MatyDSL.suspend(:cust_req_handler, state)
  end

  @spec summarise_items(map()) :: list(binary())
  def summarise_items(_items) do
    summary = ["something about item 1", "something about item 2", "something about item 3"]
    summary
  end

  @spec lookup_item(map(), binary()) :: binary()
  def lookup_item(_items, _item_id) do
    "something interesting"
  end

  @spec check_capacity(map(), binary()) :: boolean()
  def check_capacity(_items, _item_id) do
    false
  end

  @spec decrease_stock(map(), binary()) :: map()
  def decrease_stock(_items, _item_id) do
    %{}
  end

  @spec calculate_cost(map(), list(binary())) :: number()
  def calculate_cost(_items, _item_id) do
    10
  end
end
