defmodule MV6.Types do
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
  @type incomplete_session_ids :: %MapSet.t(){session_id()}

  @type access_point_state :: %{
          sessions: %{session_id() => registered_participants()},
          incomplete_session_ids: incomplete_session_ids(),
          participants: %{pid() => %{session_id() => %{role() => init_token()}}}
        }

  # examples
  defp registered_participants_example(), do: %{seller: self(), buyer1: nil, buyer2: nil}

  defp access_point_state_example() do
    %{
      sessions: %{
        "some_session_id_ref" => %{
          seller: "some_PID",
          buyer1: "some_other_PID",
          buyer2: "a_third_PID"
        },
        "some_other_session_id_ref" => %{
          seller: "some_PID",
          buyer1: "some_other_PID",
          buyer2: nil
        },
        "yet_another_session_id_ref" => %{
          seller: "another_PID",
          buyer1: nil,
          buyer2: nil
        }
      },
      incomplete_session_ids:
        MapSet.new(["some_other_session_id_ref", "yet_another_session_id_ref"]),
      participants: %{
        "some_PID" => %{
          "some_session_id_ref" => [{:seller, "some_init_token_ref"}],
          "some_other_session_id_ref" => [{:seller, "some_other_init_token_ref"}]
        }
      }
    }
  end
end
