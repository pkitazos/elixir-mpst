defmodule Shop.Participants.Customer do
  use Maty.Actor

  @role :customer

  @st {:install, "+shop:{request_items(nil).item_summary_handler}"}

  @st {:item_summary_handler, "&shop:{\
                          items(string[]).+shop:{\
                              checkout((string[],string)).payment_processing_handler\
                          }\
                      }"}

  @st {:payment_processing_handler,  "&shop:{\
                          processing_payment(nil).purchase_outcome_handler,\
                          out_of_stock(nil).end\
                      }"}

  @st {:purchase_outcome_handler, "&shop:{\
                          ok(nil).end,\
                          declined(nil).end\
                      }"}


  @impl true
  @spec on_link(pid(), maty_actor_state()) :: {:ok, maty_actor_state()}
  def on_link(ap, initial_state) do
    MatyDSL.register(
      ap,
      @role,
      [callback: :install, args: nil],
      initial_state
    )
  end

  init_handler :install, nil, state do
    MatyDSL.send(:shop, {:request_items, nil})
    MatyDSL.suspend(:item_req_handler, state)
  end


  handler :item_summary_handler, :shop, {:items, _items :: list(binary())}, state do
    MatyDSL.send(:shop, {:checkout, {["item1", "item2"], "my payment details"}})
    MatyDSL.suspend(:payment_processing_handler, state)
  end

  handler :payment_processing_handler, :shop, {:processing_payment, nil}, state do
    MatyDSL.suspend(:purchase_outcome_handler, state)
  end

  handler :payment_processing_handler, :shop, {:out_of_stock, nil}, state do
    # MatyDSL.suspend(:item_summary_handler, state)
    MatyDSL.done(state)
  end

  handler :purchase_outcome_handler, :shop, {:ok, nil}, state do
    # MatyDSL.suspend(:item_summary_handler, state)
    MatyDSL.done(state)
  end

  handler :purchase_outcome_handler, :shop, {:declined, nil}, state do
    # MatyDSL.suspend(:item_summary_handler, state)
    MatyDSL.done(state)
  end


end
