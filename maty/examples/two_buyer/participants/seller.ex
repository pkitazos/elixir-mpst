defmodule TwoBuyer.Participants.Seller do
  use Maty.Actor

  @role :seller

  @st {:title_handler, "&buyer1:{title(binary).+buyer1:{quote(number).decision_handler}}"}

  @st {:decision_handler, "&buyer2:{address(binary).+buyer2:{date(date).end, quit(unit).end}}"}

  @impl true
  @spec init_actor(pid()) :: {:ok, maty_actor_state()}
  def init_actor(ap_pid) do
    initial_state = %{sessions: %{}, callbacks: %{}}

    {:ok, updated_state} =
      register(
        ap_pid,
        @role,
        MatyDSL.init_callback(:install, ap_pid),
        initial_state
      )

    {:ok, updated_state}
  end


  init_handler :install, ap_pid :: pid(), state do
    {:ok, updated_state} =
      register(
        ap_pid,
        @role,
        MatyDSL.init_callback(:install, ap_pid),
        state
      )

    MatyDSL.suspend(:title_handler, updated_state)
  end

  handler :title_handler, :buyer1, {:title, title :: binary()}, state do
    amount = lookup_price(title)

    MatyDSL.send(:buyer1, {:quote, amount})
    MatyDSL.suspend(:decision_handler, state)
  end


  handler :decision_handler, :buyer2, {:address, addr :: binary()}, state do
    date = shipping_date(addr)

    MatyDSL.send(:buyer2, {:date, date})
    MatyDSL.done(state)
  end

  handler :decision_handler, :buyer2, {:quit, :unit }, state do
    MatyDSL.done(state)
  end


  @spec lookup_price(binary()) :: number()
  defp lookup_price(_title_str), do: 150

  @spec shipping_date(binary()) :: Date.t()
  defp shipping_date(_addr_str), do: ~D[2021-12-31]
end
