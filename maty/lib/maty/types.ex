defmodule Maty.Types do
  @type session_id :: reference()
  @type init_token :: reference()
  @type role :: atom()

  @type session_info :: %{
          id: session_id(),
          next_handler: function(),
          participants: %{role() => pid()},
          local_state: any()
        }

  @type maty_actor_state :: %{
          sessions: %{session_id() => session_info()},
          callbacks: %{init_token() => function()},
          ap_pid: pid(),
          role: role()
        }

  @type registered_participants :: %{role() => pid() | nil}
  @type incomplete_session_ids :: MapSet.t(session_id())

  @type access_point_state :: %{
          sessions: %{session_id() => registered_participants()},
          incomplete_session_ids: incomplete_session_ids(),
          participants: %{pid() => %{session_id() => %{role() => init_token()}}},
          session_context_module: module()
          # if an actor can only take part in a session with a single role why do I need to keep a map of roles to init_tokens ?
        }

  # defp example do
  #   %{
  #     sessions: %{
  #       "#Reference<0.1.2.3>" => %{
  #         seller: "#PID<0.111.0>",
  #         buyer1: "#PID<0.277.0>",
  #         buyer2: "#PID<0.340.0>"
  #       },
  #       "#Reference<2.4.6.8>" => %{
  #         seller: "#PID<0.111.0>",
  #         buyer1: "#PID<0.277.0>",
  #         buyer2: nil
  #       },
  #       "#Reference<8.7.6.5>" => %{
  #         seller: "#PID<0.626.0>",
  #         buyer1: nil,
  #         buyer2: nil
  #       }
  #     },
  #     incomplete_session_ids: MapSet.new(["#Reference<2.4.6.8>", "#Reference<8.7.6.5>"]),
  #     participants: %{
  #       "#PID<0.111.0>" => %{
  #         "#Reference<0.1.2.3>" => [{:seller, "#Reference<5.1.2.3>"}],
  #         "#Reference<2.4.6.8>" => [{:seller, "#Reference<6.0.0.1>"}]
  #       }
  #     }
  #   }
  # end
end
