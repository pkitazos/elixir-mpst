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
