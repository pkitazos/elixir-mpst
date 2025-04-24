defmodule Shop.Participants.Staff do
  use Maty.Actor

  @role :staff

  @st {:install, "adjust_stock_handler"}
  @st {:adjust_stock_handler, "&shop:{
                                  ok(nil).+shop:{\
                                    add_item((string,string,number,number)).adjust_stock_handler,\
                                    remove_item(string).adjust_stock_handler\
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
    MatyDSL.suspend(:adjust_stock_handler, state)
  end


  handler :adjust_stock_handler, :shop, {:ok, nil}, state do
    MatyDSL.send(:shop, {:add_item, {"item1", "this is a sample item", 10, 500}})
    MatyDSL.suspend(:adjust_stock_handler, state)
  end
end
