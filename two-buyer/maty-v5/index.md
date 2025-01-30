Hey Chat, I'm working on my project which involved building a tool for statically checking Elixir communication patterns. The tool will be made up of several components working together to provide developers static safety guarantees about the communication-centric part of their applications.

My tool will be relying on Scribble for the definition of global protocol and the projection of global protocols to local session types. Those local session types for each participant role in our program will be used to type-check the module corresponding with a given role. If the module successfully type-checks I can then report to the developers that their program's communications are in-fact sound.

I'll be relying on a language design for adding multiparty session types to actor languages called Maty. The language design for Maty was actually developed by my project supervisor and this project that I'm working on is actually my Master's thesis. I've discussed the language design with my supervisor extensively (though there are bound to be some gaps in my understanding) and I think we've reached a point where I should be able to begin the implementation of my tool.

Essentially what I want to do now is design some behaviour(s) that would help developers structure the communication-centric parts of their application in a way that's similar to Maty programs - we're essentially pretending that we're writing a Maty program because the way MAty programs work, allows us to statically type-check the program and be sure that the communication is safe. I'm currently still in the phase where I'm designing the behaviour and deciding what a Maty-compliant Elixir program would look like, once this phase is done, I'll move on to implementing the type-checker based on Maty's typing rules.

In case you are unfamiliar Maty is a language design that my supervisor and one of his colleagues created for enabling the use of static MPSTs in actor languages. Maty itself is a language, but it was designed to show what programming with MPSTs and actors might look like, not to be a new language used in the industry. As such my job is to make it possible to essentially structure your Elixir program to work like a Maty program which then enables static checking of communications.

I'm currently working on what I consider to be the coolest part of this project, designing what a Maty-compliant Elixir program will look like. Before I go into more detail, let me know if you have any questions and/or if you'd like me to clarify some things. I want to make sure we're on the same page before we continue

---

1.	Big Picture Goal
    - You mentioned your ultimate goal is to let developers write Elixir programs that look (and behave) like Maty programs, and then statically check those Elixir programs using MPST-based typing rules. How far do you envision taking the “Maty compliance” in terms of Elixir syntax? Will it feel like a small DSL embedded in Elixir? Or will it be more like “You must follow this set of rules, but otherwise it’s just normal Elixir code”?

2.	Granularity of the Protocol
    - In typical MPST flow, you’d define a global protocol (in your case using Scribble), then project it into local types for each role. Do you see that mapping directly 1-to-1 with separate Elixir modules for each role? E.g., if your global protocol has RoleA, RoleB, etc., will you force separate modules named something like MyApp.RoleA, MyApp.RoleB? Or do you expect to allow multiple roles to live in one module?

3.	Communication Primitives
    - Maty deals with actor communication primitives like spawn, send, and receive (or equivalents). For Elixir, do you plan on reusing the existing send and receive? Or are you wrapping them in your own “session-aware” functions/macros (like maty_send, maty_receive, etc.)?
	- How strict will the usage of these primitives be for type-checking? For instance, if someone used a raw Elixir send outside of your macros, would that break the static guarantees?

4.	Session Type Annotations
    - In a language like Maty, you might need to annotate your code to indicate the session type or the role a process is playing. Will your approach require explicit type annotations at compile time? Are you planning on generating these types from the Scribble projection and then injecting them into the code? Or is there some metaprogramming approach that can automatically infer them (maybe from macros or from the module name)?

5.	Runtime vs. Compile-Time Integration
    - Where does your static checker sit in relation to Elixir’s existing compiler? Is it a separate tool that reads Elixir source code, performs an analysis (using the local types from Scribble), and then either passes or fails? Or are you planning to integrate it into the Elixir compile pipeline (e.g., a mix task or a Dialyzer-like approach)?
	
6.	Tooling & Developer Workflow
    - Elixir developers are often used to mix tasks for building, running tests, etc. How do you envision a typical workflow? For example, “define global protocol in Scribble, generate local types, implement roles with your behaviour, run mix maty_check to ensure compliance.” Does that match your vision?

7.	Error Reporting
    - Have you put any thought yet into how to communicate type errors or protocol violations back to the developer? Sometimes MPST-based errors can be tough to interpret, especially if they come from the perspective of a “global” mismatch. Are you planning on highlighting which part of the Elixir code is out of compliance with which part of the session type?
	
8.	Evolution of Protocols
    - As developers update their code, protocols might change. Is your tool going to require them to re-run the Scribble generation step each time a global protocol is updated? Or is it fully integrated so that it happens automatically?
    
    ---

1. No it will definitely be more like a "you must follow this set of rules, but otherwise it's just normal Elixir code"

2. Yes, each role would get its own module. However each participant should be able to take part in multiple sessions (as the same role)

3. 
    a. For the time being I do plan on just using the Elixir primitives, but if it seems like there's need for this, I might be creating my own maty_ versions.
    
    b. well this depends on whether we do go with custom maty_ primitives. I'm not too bothered either way as long as there is some well defined way for developers to write their programs that gives them static guarantees you know? Like if I tell a developer "Write your program like this and you will have static communication safety guarantees" and they go an ignore my instructions then it's kinda their fault.

4. Any of those approaches is fine. I was even considering doing some code-gen from the Scribble types to scaffold the programs properly and then the developer would just have to go in and fill the modules with their business logic

5. I'm thinking it would be a Dialyzer-like approach that hooks into Elixir's existing compilation pipeline (in-fact, my tool will work under the assumption that Dialyzer was ran before my tool to get proper type annotations in the AST)

6. I've really not thought that far ahead, and I think this is something we can worry about at a later stage

7. Reaaaally have not thought this far ahead, but you raise an excellent point. Ideally I would be able to provide good human-readable error messages

8. Again not something I've thought of and also not something I think is in the scope of this tool. If I find that I've finished all other related work and have more development time, we can revisit what the best way to handle updated protocols is.


---

1.	**One Process vs. Multiple Processes per Role**
	-	In your Maty code, each role is conceptually a process. However, sometimes we might want each role to handle multiple concurrent sessions. In Elixir, that could mean:
	    -	One process per role that internally manages multiple sessions with a state map, or
	    -	One process per “instance” of a role (i.e., spawn a new process whenever we start a new session for that role).
    How do you envision it? Is each Buyer or the Seller intended to be a single long-lived process, or do you prefer spinning up a new process each time a session starts?
	
2.	**main / Application Startup**
    - In the Maty example, main spawns the Seller process, and then calls spawnBuyers. In Elixir, we usually structure things with a mix application entry point (via an OTP Application) or we might write a script that starts everything.
    - Do you imagine replicating the “main” function with some top-level function that spawns each role’s process? Or do you see each role as a standalone GenServer that can be started by a supervisor?

3.	**Handlers vs. receive**
    - In the Maty code, we have handler Buyer1 { title(x) |-> ... }, which is basically a named “receive” block that matches on the message label (title) and its payload.
    - In Elixir, do you want to replicate that idea with pattern matching in a receive do ... end block? Or do you see a scenario where each handler becomes a small callback function in the GenServer approach (like handle_info({:title, x}, state) -> ...)?

4.	**register, suspend, spawn, etc.**
    - Maty has special keywords that set up the session environment. In Elixir, we don’t have these exact keywords. We could replicate them with library calls or macros:
    - register(ap, role) could become some function like MyMaty.register(ap, role, process_pid) or something similar.
    - suspend might be replaced by “waiting for a message” or a “receive” in a loop.
    - Are you okay with us inventing a small library API or a set of macros to mirror these operations? Or do you want to keep it 100% raw OTP constructs (like GenServer.call/2, GenServer.cast/2, or raw receive)?

5.	**Maty “Choice” vs. Elixir**
    - In the protocol, Buyer2 has a choice to either send address(...) or quit(...). In standard session types, that’s a branching. In Elixir, we normally do an if/else or case. Do you want that choice to be explicit in the code (like if amount > 100, do: quit, else: address) or do you want something more domain-specific (like Maty.choice macros, etc.)?

6.	**Data Structures for Messages**
    - In your Maty program, we have messages like title(String), quote(Int), date(Date), etc. In Elixir, we’d presumably use standard tuples like {:title, x} or {:quote, amount}. Do you have any specific preference for how those messages are shaped (e.g., atoms for labels, typed payloads, etc.)?

7.	**Strict Static Checking**
    - Finally, do you plan to integrate a static checking approach right away (i.e., macros that fail to compile if you deviate from the protocol), or do you first want to see a runtime approach with typed messages and then layer in static checks later?

---

1. So in our case we would want the Seller to be a single long-lived process but the buyers would be a new process each time (emulating a real-world situation where the same seller can deal with multiple different buyers at the same time)

2. I haven't thought about supervisors yet, so for now let's just assume that we'll have a "main" function or script that sets everything up

3. Neither is quite what I'm envisioning to be honest, I'll tell you more about this in a second though once I tell you about my current plant

4. I actually don't want to rely on OTP constructs like GenServer at all, I want to define my own behaviour with its own message loop. Again I'll go into more detail about this in a moment, but I'm glad you're asking these sorts of questions. It tells me you definitely understand what we're building.

5. I'm perfectly happy for this to be an if/else or case block which just continues with the correct way (either suspending with a handler or ending the communication)

6. Yeah I was also thinking these would just be standard tuples like {:title, x} or whatever.

7. To start off I don't want to worry about the checking at all, I just want us to build out the program in a Maty-compliant way and figure out what our custom behaviour(s) will look like. Once that's all done we'll start building our static checking tool and/or macros

---

Okay so from my testing / planning / thinking so far I know that we'll definitely need to create a `MatyActor` behaviour that controls the message loop and defines some callbacks for the developer to implement in their module. So Seller, Buyer1, and Buyer2 would all be modules using the `MatyActor` behaviour.

There will also be an `AccessPoint` (perhaps this could also be a behaviour but I haven't figured out how this would work yet) actor which just keeps and updates some state. Essentially participants will register in a session under their role and then messages sent to other participants will always be "attached" to a session so that the receiving participant knows who to respond to.

Along with the required `MatyActor` callbacks, the participant modules (Seller, Buyer1, Buyer2) would also implement their handler functions which would either terminate the session with a {:done, reason} message or suspend to another handler with a {:suspend, &my_handler/1} message. Internally each participant would keep track of the "suspended" function in each session they are participating in. And inside the message loop the participant would call the handler with any received messages which would only proceed to the next step in the communication if the message matches all the requirements (correct shape, and from the correct role).

I'm not super tied to the exact shape of the state I'm storing in the actor but I imagine the message loop would look something like this:

```elixir
 defp loop(module, actor_state) do
    receive do
      {:maty_message, session_id, msg} ->
        session_info = Map.fetch!(actor_state.sessions, session_id)

        {action, next_fun, new_local_state} = apply(session_info.next_handler, [msg, session_info.local_state])

        updated_actor_state = handle_action(action, next_fun, new_local_state, session_id, actor_state)
        loop(module, updated_actor_state)
    end
  end
```

And then I would also have a handle_action function defined for each expected message shape:
```elixir
defp handle_action(:suspend, next_fun, new_local_state, session_id, actor_state) do
  put_in(actor_state, [:sessions, session_id], %{
    next_handler: next_fun,
    local_state: new_local_state
  })
end

defp handle_action(:done, _next_fun, _local_state, session_id, actor_state) do
  update_in(actor_state, [:sessions], &Map.delete(&1, session_id))
end

defp handle_action(:noreply, next_fun, new_local_state, session_id, actor_state) do
  # same as suspend but maybe a different semantics
  # perhaps it would just return the current actor_state
  put_in(actor_state, [:sessions, session_id], %{
    next_handler: next_fun,
    local_state: new_local_state
  })
end
```
(the above would both live in the `MatyActor` behaviour)

And then inside the Buyer1 module for example there would be something like:
```elixir
def quote_handler({:quote, amount, from_pid}, local_state) do
  share_amount = amount / 2
  IO.puts("Received quote=#{amount}, sending share=#{share_amount} to Buyer2")
  send(session.buyer2, {:maty_message, session_id, {:share, share_amount, self()}})
  {:done, :normal, local_state}
end
```
And the Buyer2 module would contain something like this:
```elixir
def share_handler({:share, amount, from_pid}, local_state) do
  IO.puts("Received share=#{amount}")

    if amount > 100 do
      IO.puts("share > 100, sending quit to Seller")

      send(session.seller, {:maty_message, session_id, {:quit, :unit, self()}})
      {:done, :normal, local_state}
    else
      address = get_address()
      IO.puts("share <= 100, sending address=#{address} to Seller")
      IO.puts("Suspending with 'date_handler'")

      send(session.seller, {:maty_message, session_id, {:address, address, self()}})
      {:suspend, next_handler: &__MODULE__.date_handler/2, local_state}
    end
end
```

Some of the things I haven't yet figured out are:
- what the registration flow with the AccessPoint actor would be like
- what the best way to structure the internal state of each actor would be

There's way more to figure out still I'm sure, but for now these are the main questions I have hanging over my head.

---

So I have some questions. Let's start by going through them 1 by 1. Can you explain a bit the rationale of needing a handle_message/3 callback to be defined by the user's modules? I'm just a little confused about what the handle_message function does / is used for.


---

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
