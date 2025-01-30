# Maty v4

We're now making two significant changes to our Maty program. The first one is that our `MatyRole`, really should be a `MatyActor` as it's not a description of a role in a session, but an actor in a Maty program. Related to this, our second change is to allow the *same* actor to take part in multiple sessions, compared to v3 where each actor was given a role in a single session.

The key takeaway is that an actor may have different roles in different sessions. Now, that may not necessarily be a smart way for us to organise our code, particularly for our Two-Buyer protocol example, but theoretically there's no reason an actor *couldn't* take on different roles in different sessions.

A good indicator that our new version matches the intended Maty behaviour would be having the same seller handle a purchase interaction with two distinct sets of buyers.

Another key change to the way we've organised our code so far is to do with the access point. V3 treated access points as a simple session context struct which we can initialise and pass to all participants in a session, however since we must be able to have the same seller act in different sessions, we actually cant's just have our access points be a static struct, but they themselves must be actors with which our different Maty actors can register their role in a particular session.

## MatyActor

A `MatyActor` is just a single Erlang/Elixir process that can participate in multiple sessions at once. Each session is identified by `session_id`. The local state for each session is stored in a map that might look like this:
```elixir
actor_state = %{
  sessions: %{
    "session1" => %{
      next_handler: some_function,
      local_state: some_local_state, # could be another map
    }
  },
  # there could be other fields here, but there has to at least be a `:sessions` field
}
```

Rather than having a handle_message function for each message type or having a single handle_message which pattern matches on the message with a case statement, we store in our internal state the a pointer to a function - the next_handler that we will apply when we next receive a message.
```elixir
# returns a map of all the sessions, indexed by the session_id
sessions_map = actor_state.sessions

# returns the current info stored in our actor for a particular session_id
session_info = Map.fetch!(sessions_map, session_id)

# we can now obtain our next_handler from the session_info
next_function = session_info.next_handler

# we can apply this function to the incoming message along with the local_state of this session
apply(next_fun, [msg, session_info.local_state])

# and we know that this function should be a handler, which means it should return something like
{:suspend, &another_handler/2, new_local_state}
# or
{:done, reason, new_local_state}

# so of course we can pattern match this out and handle each action appropriately
{action, next_fun, new_local_state} = apply(next_fun, [msg, session_info.local_state])

```






## Notes

So our MatyActor behaviour defines 3 callbacks that users must implement.

- init_actor
- setup_actor (optional)
- init_session

setup_actor is optional and the default implementation is a no-op. It's used to perform any actions in an actor at the start of a session before the actor suspends with its first handler

we then have 3 handle_action functions that are called by the MatyActor behaviour to handle each of the three possible message types that handlers can return. :suspend, :done, :noreply (acts like ignore)


in our event loop we look for a session using the provided id. if we don't find a session under that id we use the actors init_session function to setup a new session. the function should return a tuple of the form `{:ok, first_handler_function, local_state, new_actor_state}`. We then update our actor state with info from this new session and continue to loop.


From here things begin to get murky. It seems like the event loop would then just apply the handler to the message and state and then handle the function's return, but I don't understand how this acts the same way that our previous version (v3) did. In v3 we essentially had a state machine that we would transition between states (handlers) based on the message we received and the handler we were in. This seems to be a bit more freeform, but I'm not sure how we can guarantee that we're following the correct path through our session.

I guess that each handler in the user's code would define the handler it wants to suspend with, but I'm not sure where the rest of the message logic would live. We need a way to check that:

1. the message received has the correct shape
2. the message received is from the expected participant
3. the actor is in the correct state to receive this message

I guess I forgot that the event loop literally has a receive statement in it which blocks until a message is received that matches the shape `{:session_message, session_id, msg}` where msg would be a thing from our communication protocol. Something like `{:title, title, from_pid}` or `{:share, amount, from_pid}`. So that's how messages are handled, the other thing is that I suppose when a message is sent from one participant to the next they need to be in that format. Perhaps that's something we could use a macro for or something.

Then I just need to figure out where the recipient check lives. I basically want to ignore messages that come from incorrect participants. That should definitely be in the user handlers themselves, perhaps in the form of guards.
