# Elixir MPST

Multiparty Session Typing for Elixir apps using the Maty language design


## Timeline

- [x] Scribble Two-Buyer protocol (w0)
- [x] Elixir GenServer Two-Buyer protocol (w1)
- [x] Maty-compliant Elixir program design (w2-5)
- [x] Typechecker in Haskell (w5-7)
- [x] add dialyzer to pipeline
  - [x] Two-Buyer program
  - [x] Maty Library code
- [x] simple Elixir program AST analysis
  - [x] how do I access the AST
  - [x] what gets returned
  - [x] how do I access the fully expanded AST
  - [x] how do I inject functionality into the compilation pipeline
- [x] Maty-compliant Elixir program AST analysis
  - there is likely to be a bunch of stuff in here that I can ignore
  perhaps it would help to do a run-through of this where I throw out the stuff I don't care about
  and just look at typechecking the bits that I do care about
- [x] create manual annotations using the scribble output and stick them at the top of each role module
  the annotations should be cut up and labelled into the type for each handler
  and then that label should be used to annotate individual handlers
  these are the session types that will be used to typecheck each function
- [x] as a first step, pick one module, write out all the session type annotations, cut them up and then grab that module
  expand tf out of the code into fully expanded ASTs and start the typechecking.
  you are bound to run into problems here at which point we will re-asses
- ...
- [ ] Scribble protocol code-gen
- [ ] rewrite typechecker in Elixir
- [ ] actually typechecking the AST using the Maty typechecker
- ...
- [ ] evaluation: case studies (w11)


## Tasks

- [ ] clean up repo (move the unrelated sub-directories)
- [ ] centralise errors to dedicated module
- [ ] polish up error reporting to use meta information
- [ ] add doc comments to code
- [ ] add a couple more examples
