defmodule TwoBuyer.Participants.Buyer2 do
  alias Maty.Logger
  use Maty.Actor

  @role :buyer2

  @impl true
  def init_actor(ap_pid) do
    initial_state = %{sessions: %{}, callbacks: %{}, ap_pid: ap_pid, role: @role}

    register(
      ap_pid,
      @role,
      fn _, state -> {:suspend, &__MODULE__.share_handler/4, state} end,
      initial_state
    )
  end

  def share_handler({:share, amount}, from_pid, %{participants: participants} = session, state)
      when from_pid === participants.buyer1 do
    log(:share_handler, "Received share=#{amount}")

    if amount > 100 do
      log(:share_handler, "share > 100, sending quit to Seller")

      maty_send(
        participants.buyer2,
        participants.seller,
        session.id,
        {:quit, :unit}
      )

      {:done, :unit, state}
    else
      address = get_address()
      log(:share_handler, "share <= 100, sending address=#{address} to Seller")
      log(:share_handler, "Suspending with 'date_handler'")

      maty_send(
        participants.buyer2,
        participants.seller,
        session.id,
        {:address, address}
      )

      {:suspend, &date_handler/4, state}
    end
  end

  def share_handler(_, _, _, state), do: {:continue, nil, state}

  def date_handler({:date, date}, from_pid, %{participants: participants}, state)
      when from_pid === participants.seller do
    log(:date_handler, "Received date=#{date}, finishing.")

    {:done, :unit, state}
  end

  def date_handler(_, _, _, state), do: {:continue, nil, state}

  # -----------------------------------------------------------------

  defp get_address(), do: "18 Lilybank Gardens"

  # -----------------------------------------------------------------

  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
