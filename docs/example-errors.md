# Protocol Adherence Violations

- ~~Incorrect Action: Attempting to perform an action that is not permitted by the session type at the current state.~~

- ~~Incorrect Target Participant: Sending a message to the wrong participant role.~~


- Incorrect Handler Suspension: Suspending execution with the wrong handler name.
```elixir
  init_handler :install, ap_pid :: pid(), state do
    {:ok, updated_state} =
      MatyDSL.register(
        ...
      )
    # Incorrect: Suspending with :decision_handler instead of :title_handler
    MatyDSL.suspend(:decision_handler, updated_state)
  end
```

- Incorrect Message Label: Sending a message with the wrong label.
```elixir
  handler :quote_handler, :seller, {:quote, amount :: number()}, state do
    share_amount = amount / 2
    # Incorrect: Sending to message {:amount, :number} instead of {:share, :number}
    MatyDSL.send(:buyer2, {:amount, share_amount})
    MatyDSL.done(state)
  end
```
- ~~Incorrect Payload Type: Sending a message payload whose value type mismatches the type specified in the session type.~~

- ~~Incomplete Choice Implementation: Failing to provide handler implementations for all branches of an external choice.~~


# Framework Usage Errors:

- Missing Session Registration: Failing to MatyDSL.register with an access point within the on_link/2 callback.
```elixir
  @impl true
  @spec on_link({pid(),  binary()}, maty_actor_state()) :: {:ok, maty_actor_state()}
  def on_link({ap_pid, title}, initial_state) do
    # Incorrect: function does not register to a session
    {:ok, initial_state}
  end
```

- Invalid Init Handler: Providing an invalid initialisation handler reference or incorrect arguments during registration.
```elixir
  @impl true
  @spec on_link({pid(),  binary()}, maty_actor_state()) :: {:ok, maty_actor_state()}
  def on_link({ap_pid, _title}, initial_state) do
    MatyDSL.register(
      ap_pid,
      @role,
      # Incorrect: attempting to register with a (message) handler instead of an init_handler
      [callback: :quote_handler, args: [{:quote, 10}]],
      initial_state
    )
  end


  init_handler :install, title :: binary(), state do
    ...
  end


  handler :quote_handler, :seller, {:quote, amount :: number()}, state do
    ...
  end
```

- Use of Native Communication: Attempting to use Elixir’s built-in send/2 or receive within a Maty actor.
```elixir
  init_handler :install, title :: binary(), state do
    # Incorrect: attempting to send message using built-in Kernel.send/2
    send(self(), {:title, title})
    MatyDSL.suspend(:quote_handler, state)
  end
```