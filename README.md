# Elixir MPST

Multiparty Session Typing for Elixir apps using the Maty language design

## Tasks

<!-- typechecker --->
- [x] branches handled by separate clauses accumulated in $\Delta'$ environment
- [x] case expressions in non-handlers
- [ ] more map operations
- [x] create @init_handler annotation and typechecking clause
- [x] handler send/suspend/end macros
- [ ] add sub-typing
- [ ] get/set state
<!-- admin --->
- [ ] polish up error reporting to use meta information (function/module name, line/col numbers, remove datetime reporting)
- [ ] add `@doc` and `@moduledoc` comments to code
- [ ] split some of the larger modules into sub-modules
<!-- evaluation --->
- [ ] add a couple more examples
  - [ ] shop
  - [ ] three-buyer
  - [ ] auction
  - [ ] chat_server
  - [ ] some broken examples


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
