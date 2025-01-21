defmodule TwoBuyerMaty2.Buyer1 do
  use GenServer
  alias TwoBuyerMaty2.SessionContext
  alias TwoBuyerMaty2.Logger

  @name __MODULE__
  @role :buyer1

  def start_link() do
    GenServer.start_link(@name, %{}, name: @name)
  end

  def init_role(%SessionContext{} = session, title) do
    GenServer.cast(@name, {:init_role, session, title})
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
  def handle_cast({:init_role, session, title}, state) do
    log("Sending title=#{title} to Seller")
    log("Suspending with 'quote_handler'")
    send(session.seller, {:title, title, self()})
    {:noreply, %{state | session: session, current_handler: :quote_handler}}
  end

  @impl true
  def handle_info(msg, state) do
    case state.current_handler do
      :quote_handler -> handle_quote(msg, state)
    end
  end

  # -----------------------------------------------------------------

  defp handle_quote({:quote, amount, from_pid}, %{session: session} = state)
       when from_pid == session.seller do
    share_amount = amount / 2
    log(:quote_handler, "Received quote=#{amount}, sending share=#{share_amount} to Buyer2")
    send(session.buyer2, {:share, share_amount, self()})
    {:stop, :normal, state}
  end

  defp handle_quote({:quote, _amount, from_pid}, state) do
    log(:quote_handler, "Ignoring 'quote' from pid=#{inspect(from_pid)}")
    {:noreply, state}
  end

  defp handle_quote(_other_msg, state) do
    log(:quote_handler, "Unexpected message")
    {:noreply, state}
  end

  # -----------------------------------------------------------------

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
