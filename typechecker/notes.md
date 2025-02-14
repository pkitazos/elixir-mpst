
## Typechecker Functions

```haskell
tcVal :: Env -> Val -> Either Error Type

tcExpr :: Env -> ST -> Expr -> Either Error (Type, ST)
```

### Value Typing

**TV-LAM**  
If `M` is well-typed as a computation of type `B` when `x` is available as type `A` and the session transitions from `S` to `T`, then `λ x. M` is a function from `A` to `B` (with session precondition `S` and postcondition `T`).


**TV-HANDLER**  
`handler p {l_i(x_i) -> M_i}_i` the handler is typable with type `Handler(p&{l_i(A_i).S_i}_i)` if each continuation `M_i` is typable with session precondition `S_i` where the environment is extended with `x_i` of type `A_i`, and all branches have the postcondition `end`.


### Computation Typing

**T-APP**  
A function application `V W` is typable provided that the precondition in the function type matches the current precondition, and advances the postcondition to that of the function type.

**T-SPAWN**  
The `spawn M` term spawns a new actor that evaluates term `M`; this rule types the expression with the unit type if the spawned thread `M` must has return type `1` and pre- and postconditions `end` (since the spawned computation is not yet in a session and so cannot communicate)


**T-SEND**  
This rule types a send computation `p ! l(V)` if `l` is contained within the selection session precondition, and if `V` has the corresponding type; the postcondition is the session continuation for the specified branch


**T-SUSPEND**  
When an actor wishes to receive a message, it must suspend itself and install a message handler using `suspend V`. This rule states that `suspend V` is typable if the handler is compatible with the current session type precondition; since the computation does not return, it can be given an arbitrary return type and postcondition.


**T-NEWAP**  
Sessions are initiated using access points: we create an access point for a session with roles and types `(p_i : s_i)_i` using `newAP(p_i:s_i)_i`, which must annotated with the set of roles and local types to be involved in the session. The rule ensures that the session types satisfy a safety property; not implemented for now, but at a high level, if a set of session types is safe then the types are guaranteed never to cause a runtime type error due to a communication mismatch.

**T-REGISTER**  
An actor can register to take part in a session as role `p` on access point `V` using `register V p M`; term `M` is a callback to be invoked once the session is established. This rule ensures that the access point must contain a session type `T` associated with role `p`, and since the initiation callback will be evaluated when the session is established, `M` must be typable under session type `T`. Since neither newAP nor register perform any communication, the session types are unaltered.
