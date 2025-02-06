defmodule MV6.Seller do
  alias MV6.Logger
  use MV6.MatyActor

  @role :seller

  @type session_id :: String.t()
  @type session_context :: %{any() => pid()}
  @type session_info :: %{
          id: session_id(),
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
      | next_handler: &title_handler/4,
        local_state: %{}
    }

    updated_sessions = Map.put(sessions, session_id, updated_session_info)
    log("Initialising session with id=#{session_id}")
    log("Suspending with 'title_handler'")

    {updated_session_info, %{actor_state | sessions: updated_sessions}}
  end

  def title_handler({:title, title}, from_pid, %{participants: participants} = session, state)
      when from_pid === participants.buyer1 do
    amount = lookup_price(title)
    log(:title_handler, "Received title=#{title}, sending quote=#{amount} to Buyer1")
    log(:title_handler, "Suspending with 'decision_handler'")

    maty_send(
      participants.seller,
      participants.buyer1,
      session.id,
      {:quote, amount}
    )

    {:suspend, &decision_handler/4, state}
  end

  def title_handler(_, _, _, state), do: {:continue, nil, state}

  def decision_handler({:address, addr}, from_pid, %{participants: participants} = session, state)
      when from_pid === participants.buyer2 do
    date = shipping_date(addr)
    log(:decision_handler, "Received address=#{addr}, sending date=#{date} to Buyer2")

    maty_send(
      participants.seller,
      participants.buyer2,
      session.id,
      {:date, date}
    )

    {:done, :unit, state}
  end

  def decision_handler({:quit, _}, from_pid, %{participants: participants}, state)
      when from_pid === participants.buyer2 do
    {:done, :unit, state}
  end

  def decision_handler(_, _, _, state), do: {:continue, nil, state}

  # -----------------------------------------------------------------

  defp lookup_price(_title_str), do: 150
  defp shipping_date(_addr_str), do: "2021-12-31"

  # -----------------------------------------------------------------

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
