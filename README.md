# Elixir MPST

Multiparty Session Typing for Elixir apps using the Maty language design


## Timeline

- [x] Scribble Two-Buyer protocol (w0)
- [x] Elixir GenServer Two-Buyer protocol (w1)
- [x] Maty-compliant Elixir program design (w2-5)
- [x] Typechecker in Haskell (w5-7)
- [ ] add dialyzer to pipeline
  - simple example program
  - Two-Buyer program
  - Maty Library code
- [ ] simple Elixir program AST analysis
  - how do I access the AST
  - what gets returned
  - how do I access the fully expanded AST
  - how do I inject functionality into the compilation pipeline
- [ ] Maty-compliant Elixir program AST analysis
  - there is likely to be a bunch of stuff in here that I can ignore
  perhaps it would help to do a run-through of this where I throw out the stuff I don't care about
  and just look at typechecking the bits that I do care about
- [ ] create manual annotations using the scribble output and stick them at the top of each role module
  the annotations should be cut up and labelled into the type for each handler
  and then that label should be used to annotate individual handlers
  these are the session types that will be used to typecheck each function
- [ ] as a first step, pick one module, write out all the session type annotations, cut them up and then grab that module
  expand tf out of the code into fully expanded ASTs and start the typechecking.
  you are bound to run into problems here at which point we will reasses
- ...
- [ ] Scribble protocol code-gen
- [ ] rewrite typechecker in Elixir
- [ ] actually typechecking the AST using the Maty typechecker
- ...
- [ ] evaluation: case studies (w11)
