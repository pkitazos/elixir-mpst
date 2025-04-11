# Elixir MPST

Multiparty Session Typing for Elixir apps using the Maty language design

## Tasks

- [ ] make sure 

- [ ] branches handled by separate clauses accumulated in Π environment
- [ ] case expressions in non-handlers
- [ ] more map operations

- [ ] create @init_handler annotation and typechecking clause
- [ ] handler send/suspend/end macros
- [ ] add sub-typing
- [ ] get/set state

- [ ] polish up error reporting to use meta information (function/module name, line/col numbers, remove datetime reporting)
- [ ] add `@doc` and `@moduledoc` comments to code
- [ ] add a couple more examples
  - [ ] shop
  - [ ] three-buyer
  - [ ] auction
  - [ ] chat_server
  - [ ] some broken examples
- [ ] split generic typechecking and session typechecking into separate modules



## Questions

- How do I cut up a recursive session type? 

tl;dr - unfold once and inline


## Dev things

### How to run Scribble

To check that a global protocol is correct:
```
scribble path/to/protocol.scr
```


To get a local projection of a global protocol:
```
scribble path/to/protocol.scr -project GlobalProtocolName LocalRoleName
```
