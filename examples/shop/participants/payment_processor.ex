defmodule Shop.Participants.PaymentProcessor do
  use Maty.Actor

  @role :payment_processor

  @st {:install, "transaction_handler"}
  @st {:transaction_handler, "&shop:{buy((string,number)).+shop:{\
                                  ok(nil).transaction_handler,\
                                  declined(nil).transaction_handler\
                                }\
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
    MatyDSL.suspend(:transaction_handler, state)
  end

  handler :transaction_handler, :shop, {:buy, {_details :: binary(), cost :: number()}}, state do
    if cost < 20 do
      MatyDSL.send(:shop, {:ok, nil})
    else
      MatyDSL.send(:shop, {:declined, nil})
    end
    MatyDSL.suspend(:transaction_handler, state)
  end

end
