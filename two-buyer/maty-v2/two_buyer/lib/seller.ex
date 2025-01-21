defmodule TwoBuyerMaty2.Seller do
  use GenServer
  alias TwoBuyerMaty2.SessionContext
  alias TwoBuyerMaty2.Logger

  @name __MODULE__
  @role :seller

  def start_link() do
    GenServer.start_link(@name, %{}, name: @name)
  end

  def init_role(%SessionContext{} = session) do
    GenServer.cast(@name, {:init_role, session})
  end

  def install() do
    GenServer.call(@name, :install)
  end

  # -----------------------------------------------------------------

  @impl true
  def init(_init_arg) do
    # we'll store two things in the state:
    #   1. session: references to the Buyer1, Buyer2 PIDs
    #   2. current_handler: which "handler" is currently active
    {:ok, %{session: nil, current_handler: nil}}
  end

  @impl true
  def handle_cast({:init_role, session}, state) do
    {:noreply, %{state | session: session}}
  end

  @impl true
  def handle_call(:install, _from, state) do
    # in our Maty program the `install` logic says we need to `suspend` with the `titleHandler`
    log("Suspending with 'title_handler'")
    {:reply, :ok, %{state | current_handler: :title_handler}}
  end

  @impl true
  def handle_info(msg, state) do
    # dispatch to whichever "handler" is active
    case state.current_handler do
      :title_handler -> handle_title(msg, state)
      :decision_handler -> handle_decision(msg, state)
    end
  end

  # -----------------------------------------------------------------
  # each "handler" function below can emulate Maty `handlers`
  # they return either {:noreply, new_state} or {:stop, reason, new_state}.
  # if they need to "suspend" -> they just set `current_handler` to the next handler.
  # -----------------------------------------------------------------

  defp handle_title({:title, title, from_pid}, %{session: session} = state)
       when from_pid == session.buyer1 do
    amount = lookup_price(title)
    log(:title_handler, "Received title=#{title}, sending quote=#{amount} to Buyer1")
    log(:title_handler, "Suspending with 'decision_handler'")

    # In Maty, we do: `suspend decisionHandler`
    # So we just set the current_handler to :decision_handler
    send(session.buyer1, {:quote, amount, self()})
    {:noreply, %{state | current_handler: :decision_handler}}
  end

  defp handle_title({:title, _title, from_pid}, state) do
    # This means it was a :title message from someone who’s not the real Buyer1
    log(:title_handler, "Ignoring 'title' from pid=#{inspect(from_pid)}")
    {:noreply, state}
  end

  defp handle_title(_other_msg, state) do
    # handles unexpected messages - in this case we're just logging them but we could also do nothing
    # this may not be necessary at all
    log(:title_handler, "Unexpected message")
    {:noreply, state}
  end

  defp handle_decision({:address, addr, from_pid}, %{session: session} = state)
       when from_pid == session.buyer2 do
    date = shipping_date(addr)
    log(:decision_handler, "Received address=#{addr}, sending date=#{date} to Buyer2")
    send(session.buyer2, {:date, date, self()})

    # possibly end or loop back to :install
    {:stop, :normal, state}
  end

  defp handle_decision({:address, _addr, from_pid}, state) do
    log(:decision_handler, "Ignoring 'address' from pid=#{inspect(from_pid)}")
    {:noreply, state}
  end

  defp handle_decision({:quit, _, from_pid}, %{session: session} = state)
       when from_pid == session.buyer2 do
    log(:decision_handler, "Received quit, stopping.")
    {:stop, :normal, state}
  end

  defp handle_decision({:quit, _, from_pid}, state) do
    log(:decision_handler, "Ignoring 'quit' from pid=#{inspect(from_pid)}")
    {:stop, :normal, state}
  end

  defp handle_decision(_other_msg, state) do
    log(:decision_handler, "Unexpected message")
    {:noreply, state}
  end

  # -----------------------------------------------------------------
  # Utility stubs
  # -----------------------------------------------------------------
  defp lookup_price(_title_str), do: 155
  defp shipping_date(_addr), do: "2025-02-07"

  # -----------------------------------------------------------------
  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
