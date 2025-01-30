defmodule TwoBuyerMaty4.Buyer1 do
  alias TwoBuyerMaty4.{MatyActor, Logger}

  use MatyActor

  @name __MODULE__
  @role :buyer1

  def init_actor(_args) do
    {:ok, %{sessions: %{}}}
  end

  def register(_session_id, _role, _ap_pid, actor_state) do
    # ? registers with access point
    {:ok, actor_state}
  end

  # ------------------------------------------------------------------

  def send_title_handler({:send_title, title, _from_pid}, local_state) do
    session_id = "session1"

    send(seller, {:session_message, session_id, {:title, title, self()}})

    session_info = actor_state.sessions[session_id]

    updated_actor_state =
      put_in(new_actor_state, [:sessions, session_id], %{
        next_handler: &quote_handler/2,
        local_state: [{:book_title, title}]
      })

    {:ok, updated_actor_state}
  end

  def quote_handler({:quote, amount, from_pid}, local_state) do
    share_amount = amount / 2
    log(:quote_handler, "Received quote=#{amount}, sending share=#{share_amount} to Buyer2")

    send(session.buyer2, {:session_message, session_id, {:share, share_amount, self()}})
    {:stop, :normal, state}
  end

  # ------------------------------------------------------------------

  defp log(msg), do: Logger.log(@role, msg)
  defp log(handler, msg), do: Logger.log(@role, handler, msg)
end
