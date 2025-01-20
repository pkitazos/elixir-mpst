defmodule TwoBuyerMaty2.Buyer1 do
  use GenServer

  @name __MODULE__

  def start_link() do
    GenServer.start_link(@name, %{}, name: @name)
  end

  def init_role(%TwoBuyerMaty2.SessionContext{} = session) do
    GenServer.cast(@name, {:init_role, session})
  end

  def send_title(title) do
    GenServer.cast(@name, {:send_title, title})
  end

  # -----------------------------------------------------------------

  @impl true
  def init(_init_arg) do
    # we'll store two things in the state:
    #   1. session: references to the Buyer2, Seller PIDs
    #   2. current_handler: which "handler" is currently active
    {:ok, %{session: nil, current_handler: nil}}
  end

  @impl true
  def handle_cast({:init_role, session}, state) do
    {:noreply, %{state | session: session}}
  end

  @impl true
  def handle_cast({:send_title, title}, %{session: session} = state) do
    IO.puts("[Buyer1] Sending title=#{title} to Seller, suspending with 'quote_handler'")
    send(session.seller, {:title, title})
    # Can either do it this way where send_title is a separate operation
    # but if we wanted to be true to the way it's written out in the MAty program
    # the send_title operation actually happens in the `buyer1` function
    # so the `Seller ! title(title)` would actually happen in the above handler
    {:noreply, %{state | current_handler: :quote_handler}}
  end

  @impl true
  def handle_info(msg, state) do
    case state.current_handler do
      :quote_handler -> handle_quote(msg, state)
    end
  end

  # -----------------------------------------------------------------

  defp handle_quote({:quote, amount}, %{session: session} = state) do
    IO.puts("[Buyer1] Received quote=#{amount}. Forwarding share to Buyer2...")

    buyer2 = session.buyer2
    share_amount = amount / 2
    send(buyer2, {:share, share_amount})
    {:stop, :normal, state}
  end

  defp handle_quote(_other_msg, state) do
    IO.puts("[Buyer1] (quote_handler) got unexpected message")
    {:noreply, state}
  end
end
