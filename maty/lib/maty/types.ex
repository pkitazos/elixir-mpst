defmodule Maty.Types do
  @type session_id :: reference()
  @type init_token :: reference()
  @type role :: atom()

  @type session :: %{
          id: session_id(),
          # update handler state for a given session to be a map of roles to functions
          # then when I try to invoke a handler I do so only if the message I received came from the expected participant
          #   %{my_role => {function_for_handling_next_message, message_author_role}}
          handlers: %{role() => {function(), role()}},
          participants: %{role() => pid()},
          local_state: any()
        }

  @type maty_actor_state :: %{
          sessions: %{session_id() => session()},
          callbacks: %{init_token() => {role(), function()}}
        }

  @type access_point_state :: %{
          participants: %{role() => [{pid(), init_token()}]}
        }
end
