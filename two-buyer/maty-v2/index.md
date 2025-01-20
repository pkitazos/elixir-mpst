# Maty V2

In order to effectively mimic the way `suspend` works in Maty, we'll keep track of a `current_handler` in our GenServer state. When a handler or process suspends with a handler we will set that handler as our `current_handler`.

Then all calls will be routed through a standard GenServer `handle_info` call which will only give control to a handler if it matches what's stored in the `current_handler` state.




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