```
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


```elixir
def title_handler({:title, x}, from, session, state) when from === session.buyer1 do
    maty_send(session, :buyer1, {:quote, lookup_price(x)})
    {:suspend, &decision_handler/1, state}
end

def title_handler(_msg, _from, _session, state) do
    {:ignore, state}
end

def decision_handler({:address, addr}, from, session, state) when from === session.buyer2 do
    maty_send(session, :buyer2, {:date, shipping_date(addr)})
    {:done, state}
end

def decision_handler({:quit, _addr_}, from, session, state) when from === session.buyer2 do
    {:done, state}
end

def decision_handler(_msg, _from, _session, state) do
    {:ignore, state}
end
```


```elixir
handler :title_handler, from: :buyer1 do
  title(x) -> 
    buyer1 ! {:quote, lookup_price(x)}
    suspend :decision_handler
end

handler :decision_handler, from: :buyer2 do
  address(addr) ->
    buyer2 ! {:date, shipping_date(addr)}
    done(:normal)

  quit(_) ->
    done(:normal)
end
```


```elixir
handler :title_handler, from: :buyer1, msg, session, state do
  case msg do
    {:title, x} -> 
        maty_send(session, :buyer1, {:quote, lookup_price(x)})
        {:suspend, :decision_handler, state}
  end
end

handler :decision_handler, from: :buyer2, msg, session, state do
  case msg do
    {:address, addr} ->
        maty_send(session, :buyer2, {:date, shipping_date(addr)})
        {:done, :normal, state}

    {:quit, _} ->
        {:done, :normal, state}
  end
end
```


# Notes

## AP, registration, initialisation

So, API looks good, however the way to handle the synchronisation of different participants registering with the AP needs to be revised. 

When we initialise an actor we should register with the access point and provide an initialisation token and a callback. The registration process should be a fire-and-forget. The register function should live in the MatyActor module. It should define a ref / init_token and associate that to the supplied callback in the actor state as well.

The init_token should also be sent to the AP. Once enough (as in all the necessary roles for a valid session) participants register with the AP, the AP should automatically send a message to all participants informing them of all other participant PIDs. This message should include the `init_token` associated with this role. In the `MatyActor` loop the actor should lookup the callback stored for this init_token and invoke the callback (which should behave similar to a handler, and then finally suspend with the correct handler (perhaps using the `init_session` function, this is more of an implementation detail))

The key difference from our current approach is that the Access Point itself needs to track the completeness of each session in terms of the registered participants and once everyone has successfully registered, it will send a message to all the participants in this newly initialised session. This message that each `MatyActor` will receive will trigger the stored callback which will kick-start our interaction.

In our Two-Buyer example, this solves two issues we previously had: 
1) We're able to do the recursive install (more on that soon)
2) We no longer need an `initial_action_handler` in our Buyer1 (which didn't exist at all in the Maty code). We simply store a callback that will send a message to a seller with a title, once the session starts.

## recursive install

As we just discussed, registration happens inside the `init_actor` function and a callback is supplied at initialisation. Once all participants have registered, the event loop will trigger the stored callback.

In our Two-Buyer maty example, the seller initialisation looks like this:
```
seller(ap) ===
    rec install(_) . 
        register ap Seller (install (); suspend titleHandler)
```
Our supervisor defined the install function with pseudo-code as such:
```
fn install() {
  register ap Seller (install(); suspend titleHandler)
}
```
Which in Elixir, might look like this:
```elixir
  def install() do
    Seller.register(ap, @role, fn state ->
      install()
      {:suspend, &title_handler/4, state}
    end)
  end
```
So what's going on here? Well, the install function is the callback we will supply to our registration function, like so:
```elixir
Seller.register(ap, :seller, init_token, &install/0)
```
Once we receive the `{:init_session, init_token}` message from the ap we will retrieve the callback which corresponds to the supplied `init_token` and invoke the function. In this case the function that will be invoked is the `install/0` function. Which itself will re-register the seller with the access point and supply the install functions as *its* callback, and so on.

## macros + other niceties

Having a `handler` macro would in-fact be preferable to simply using regular functions (though I'm not sure how we will reference our handlers as we won't really be able to do `&title_handler/4` anymore (which is a feature my Supervisor really liked), but we'll figure this out later on).

Macros are a very powerful feature that Elixir provides us with which is one of the benefits of doing this project in Elixir rather than Erlang, so we are free to use them where they would make sense.