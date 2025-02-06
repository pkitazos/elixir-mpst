defmodule MV6.Buyer1 do
  alias MV6.Logger
  use MV6.MatyActor

  @role :buyer1

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

  def send_title(pid, session_id, title) do
    IO.puts("[DEBUG] sending title to session #{session_id}")
    maty_send(pid, pid, session_id, {:send_title, title})
  end

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
      | next_handler: &initial_action_handler/4,
        local_state: %{}
    }

    updated_sessions = Map.put(sessions, session_id, updated_session_info)
    log("Initialising session with id=#{session_id}")
    log("Suspending with 'initial_action_handler'")

    {updated_session_info, %{actor_state | sessions: updated_sessions}}
  end

  def initial_action_handler(
        {:send_title, title},
        from_pid,
        %{participants: participants} = session,
        state
      )
      when from_pid === participants.buyer1 do
    log(:initial_action_handler, "Received title=#{title}, sending to Seller")
    log(:initial_action_handler, "Suspending with 'quote_handler'")

    maty_send(
      participants.buyer1,
      participants.seller,
      session.id,
      {:title, title}
    )

    {:suspend, &quote_handler/4, state}
  end

  def initial_action_handler(_, _, _, state), do: {:continue, nil, state}

  def quote_handler({:quote, amount}, from_pid, %{participants: participants} = session, state)
      when from_pid === participants.seller do
    share_amount = amount / 2
    log(:quote_handler, "Received quote=#{amount}, sending share=#{share_amount} to Buyer2")
    log(:quote_handler, "Suspending with 'decision_handler'")

    maty_send(
      participants.buyer1,
      participants.buyer2,
      session.id,
      {:share, share_amount}
    )

    {:done, :unit, state}
  end

  # -----------------------------------------------------------------

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
