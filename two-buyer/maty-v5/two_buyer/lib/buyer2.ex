defmodule TwoBuyerMaty5.Buyer2 do
  alias TwoBuyerMaty5.Logger
  use TwoBuyerMaty5.MatyActor

  @role :buyer2

  @type session_id :: String.t()
  @type session_context :: %{any() => pid()}
  @type session_info :: %{
          next_handler: function(),
          participants: session_context(),
          local_state: any()
        }
  @type actor_state :: %{
          sessions: %{session_id() => session_info :: session_info()},
          ap_pid: pid()
        }

  # ------------------------------------------------------------------

  @impl true
  def init_actor(ap_pid) do
    initial_state = %{sessions: %{}, ap_pid: ap_pid, role: @role}

    {:ok, initial_state}
  end

  @impl true
  def init_session(session_id, %{sessions: sessions} = actor_state) do
    partial_session_info = Map.get(sessions, session_id)

    updated_session_info = %{
      partial_session_info
      | next_handler: &share_handler/4,
        local_state: %{}
    }

    updated_sessions = Map.put(sessions, session_id, updated_session_info)

    log("Initialising session with id=#{session_id}")
    log("Suspending with 'share_handler'")

    {updated_session_info, %{actor_state | sessions: updated_sessions}}
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

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
