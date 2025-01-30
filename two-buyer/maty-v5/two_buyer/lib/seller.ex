defmodule Seller do
  @type session_id :: String.t()

  @type internal_state :: %{
          access_point_pid: pid(),
          sessions: %{session_id => %SessionContext{}}
        }

  def register(ap, session_id) do
    {:ok, session_id} = AccessPoint.register(ap, session_id, :seller, self())

    {:ok, self()}
  end
end
