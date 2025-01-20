# Maty V2

In order to effectively mimic the way `suspend` works in Maty, we'll keep track of a `current_handler` in our GenServer state. When a handler or process suspends with a handler we will set that handler as our `current_handler`.

Then all calls will be routed through a standard GenServer `handle_info` call which will only give control to a handler if it matches what's stored in the `current_handler` state.

## Maty in Elixir
<!-- This is now out of date -->
Let's look at a more concrete example to understand how the Elixir program gets mapped to a maty program

### Seller

Below we have the code for the Seller participant in our communication. We see two different kinds of functions here.

- the `seller` function which initialises the Seller by running some install logic and suspending with the `titleHandler`

- two different handlers (`titleHandler` and `decisionHandler`) which each deal with the protocol at different points by blocking this process until a message with the correct format (from the correct participant) is received.

Handlers can either suspend to another handler (as seen in the `titleHandler`) or can end the communication (as seen in the `decisionHandler`)

```
seller(ap) ===
    rec install(_) . 
        register ap Seller (install (); suspend titleHandler)


titleHandler ===
    handler Buyer1 {
        title(x) |->
            Buyer1 ! quote(lookupPrice(x));
            suspend decisionHandler
    }

decisionHandler ===
    handler Buyer2 {
        address(addr) |-> Buyer2 ! date(shippingDate(addr))
        quit(_) |-> return ()
    }
```

Below we see the code as written in Elixir which mimics the logic expressed above, by explicitly keeping track of and setting the currently suspended handler.

```elixir
@impl true
def handle_cast({:init_role, session}, state) do
  {:noreply, %{state | session: session}}
end

@impl true
def handle_call(:install, _from, state) do
  # in our Maty program the `install` logic says we need to `suspend` with the `titleHandler`
  IO.puts("[Seller] Suspending with 'title_handler'")
  {:reply, :ok, %{state | current_handler: :title_handler}}
end

@impl true
def handle_info(msg, state) do
  # dispatch to whichever "handler" is active
  case state.current_handler do
    :title_handler -> handle_title(msg, state)
    :decision_handler -> handle_decision(msg, state)
  end
end
```

Here we see three handlers which each perform a slightly different kind of action. 

- All participants will always have a `handle_cast({:init_role, session}, state)` function which is the equivalent to the `seller` function. This initialises the participant by giving them access to the session context, running any set-up logic and suspending with the correct first handler.

- In the case of the Seller we also need a special public `install` function for starting the protocol, which has a corresponding `handle_call/3` function that runs any install logic (like spawning a new process to handle incoming requests) and then suspends with the correct first handler.

- All participants will also have a `handle_info(msg, state)` function which acts as a dispatcher, when a message is received the dispatcher will call the handler function corresponding to the current handler stored in the participant's state.


### Buyer1

Next let's look at the Buyer1 participant which has two functions, the initialising function and a single handler.

```
buyer1(ap, title) === 
    register ap Buyer1 (Seller ! title(title); suspend quoteHandler)

quoteHandler ===
    handler Seller {
        quote(amount) |-> Buyer2 ! share(amount/2)
    }
```

The `buyer1` function sends a message `title(title)` to the Seller and then suspends with the `quoteHandler`. Below we see the Elixir code which performs the same operations:

```elixir
  @impl true
  def handle_cast({:init_role, session}, state) do
    {:noreply, %{state | session: session}}
  end

  @impl true
  def handle_cast({:send_title, title}, %{session: session} = state) do
    IO.puts("[Buyer1] Sending title=#{title} to Seller, suspending with 'quote_handler'")
    send(session.seller, {:title, title})

    {:noreply, %{state | current_handler: :quote_handler}}
  end

  @impl true
  def handle_info(msg, state) do
    case state.current_handler do
      :quote_handler -> handle_quote(msg, state)
    end
  end
```

- Like above we have a `handle_cast({:init_role, session}, state)` function which does the initialisation and a `handle_info(msg, state)` function which acts as a dispatcher for the handlers.

- Because the Buyer1 participant needs a public function to send a title to the Seller, there is a corresponding `handle_cast/2` function for performing that operation and suspending with the correct first handler.


### Buyer2

Finally, let's look at the Maty code for the Buyer2 participant, which has one initialiser function and two handlers.

```
buyer2(ap) ===
    register ap Buyer2 (suspend shareHandler)

shareHandler ===
    handler Buyer {
        share(amount) |-> 
            if (amount > 100) then
                Seller ! quit(())
            else
                Seller ! address("18 Lilybank Gardens");
                suspend dateHandler
    }

dateHandler ===
    handler Seller {
        date(date) |-> log(date)
    }
```

And below the corresponding Elixir code:

```elixir
  @impl true
  def handle_cast({:init_role, session}, state) do
    IO.puts("[Buyer2] Suspending with 'share_handler'")
    {:noreply, %{state | session: session, current_handler: :share_handler}}
  end

  @impl true
  def handle_info(msg, state) do
    # dispatch to whichever "handler" is active
    case state.current_handler do
      :share_handler -> handle_share(msg, state)
      :date_handler -> handle_date(msg, state)
    end
  end
  ```

In this case there was no need for a separate handle_cast function because the initialiser function performs no logic in the Maty program.



## How to run

First we have to compile our mix project:
```
mix do deps.get, compile
```

Then we can load our application into iex:
```
iex -S mix
```

Finally, we can start start out program from the Main module inside iex:
```
iex> TwoBuyerMaty2.Main.start_two_buyer()
```

To recompile the application we can just use inside iex:
```
iex> recompile()
```

## TODO 

- [ ] add role guards to each Maty handler
- [ ] add participant PID to each send operation