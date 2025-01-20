defmodule TwoBuyerMaty2.Seller do
  use GenServer

  @name __MODULE__

  def start_link() do
    GenServer.start_link(@name, %{}, name: @name)
  end

  def init_role(%TwoBuyerMaty2.SessionContext{} = session) do
    GenServer.cast(@name, {:init_role, session})
  end

  def install() do
    GenServer.call(@name, :install)
  end

  # -----------------------------------------------------------------

  @impl true
  def init(_init_arg) do
    # we'll store two things in the state:
    #   1. session: references to the Buyer1, Buyer2 PIDs, etc.
    #   2. current_handler: which "handler" is currently active
    {:ok, %{session: nil, current_handler: nil}}
  end

  @impl true
  def handle_cast({:init_role, session}, state) do
    {:noreply, %{state | session: session, current_handler: :install}}
  end

  @impl true
  def handle_call(:install, _from, state) do
    # in our Maty program the `install` logic says we need to `suspend` with the `titleHandler`
    IO.puts("[Seller] In 'install' state, switching to 'title_handler'")
    {:reply, :ok, %{state | current_handler: :title_handler}}
  end

  @impl true
  def handle_info(msg, state) do
    # dispatch to whichever "handler" is active
    case state.current_handler do
      :title_handler ->
        handle_title(msg, state)

      :decision_handler ->
        handle_decision(msg, state)
    end
  end

  # -----------------------------------------------------------------
  # each "handler" function below can emulate Maty `handlers`
  # they return either {:noreply, new_state} or {:stop, reason, new_state}.
  # if they need to "suspend" -> they just set `current_handler` to the next handler.
  # -----------------------------------------------------------------

  defp handle_title({:title, x}, %{session: session} = state) do
    IO.puts("[Seller] (title_handler) received title=#{x}. Sending quote...")
    send(session.buyer1, {:quote, lookup_price(x)})

    # In Maty, we do: `suspend decisionHandler`
    # So we just set the current_handler to :decision_handler
    IO.puts("[Seller] In 'title_handler' state, switching to 'decision_handler'")
    {:noreply, %{state | current_handler: :decision_handler}}
  end

  defp handle_title(_other_msg, state) do
    # handles unexpected messages - in this case we're just logging them but we could also do nothing
    # this may not be necessary at all
    IO.puts("[Seller] (title_handler) got unexpected message")
    {:noreply, state}
  end

  defp handle_decision({:address, addr}, %{session: session} = state) do
    IO.puts("[Seller] (decision_handler) got address=#{addr}, sending date...")
    send(session.buyer2, {:date, shipping_date(addr)})

    # possibly end or loop back to :install
    {:stop, :normal, state}
  end

  defp handle_decision({:quit, _}, state) do
    IO.puts("[Seller] (decision_handler) got quit, stopping.")
    {:stop, :normal, state}
  end

  defp handle_decision(_other_msg, state) do
    IO.puts("[Seller] (decision_handler) got unexpected message")
    {:noreply, state}
  end

  # -----------------------------------------------------------------
  # Utility stubs
  # -----------------------------------------------------------------
  defp lookup_price(_title_str), do: 155
  defp shipping_date(_addr), do: "2025-02-07"
end
