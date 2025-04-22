defmodule TwoBuyer.Participants.Buyer2 do
  use Maty.Actor

  @role :buyer2

  @st {:install, "share_handler"}
  @st {:share_handler, "&buyer1:{share(number).+seller:{address(binary).date_handler, quit(nil).end}}"}
  @st {:date_handler, "&seller:{date(date).end}"}


  @impl true
  @spec on_link(pid(), maty_actor_state()) :: {:ok, maty_actor_state()}
  def on_link(ap_pid, initial_state) do
    MatyDSL.register(
      ap_pid,
      @role,
      [callback: :install, args: nil],
      initial_state
    )
  end


  init_handler :install, nil, state do
    MatyDSL.suspend(:share_handler, state)
  end


  handler :share_handler, :buyer1, {:share, amount :: number()}, state do
    if amount > 100 do
      MatyDSL.send(:seller, {:quit, nil})
      MatyDSL.done(state)
    else
      address = get_address()

      MatyDSL.send(:seller, {:address, address})
      MatyDSL.suspend(:date_handler, state)
    end
  end


  handler :date_handler, :seller, {:date, _date :: Date.t()}, state do
    MatyDSL.done(state)
  end


  @spec get_address() :: binary()
  defp get_address(), do: "18 " <> "Lilybank Gardens"
end
