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
          callbacks: %{init_token() => {role(), function()}}
          # a Maty actor does not have a "global" role it only has a role in a session
          # it also does not need to know where the ap is at all times
          # once its done registering it can throw that pid away
          # what the actor does have is a role associated with a token
        }

  @type access_point_state :: %{
          participants: %{role() => [{pid(), init_token()}]}
          # rather than keeping all that complicated state and knowing about a bunch of sessions
          # we just keep three queues that store {pid, token} pairs
          # once all three queues are not empty, we can let the participants know and discard that state
          # session ids are generated at this point
          # that means we don't need a struct of any kind to build "new sessions"
          # and we also don't need to store a set of incomplete sessions
        }
end
