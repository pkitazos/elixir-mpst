---
tags:
  - v1
---

### `BindParams`
`BindParams(p_{args}, \widetilde{T_{spec}})` → `Γ_{args}` or `Error`

- **Input:** Sequence of parameter patterns `p_{args} = (p_1, ..., p_n)`, Sequence of spec types `\widetilde{T_{spec}} = (T_1, ..., T_n)`.
- **Check Arity:** Verify `length(p_{args}) == length(\widetilde{T_{spec}})`. If not, return `Error`.
- **Process:** Initialize `Γ_{args} = ·`. For each `i` from 1 to `n`:
    - Use the pattern matching judgement to check `⊢ p_i : T_i ⟹ Γ_i`. If it fails, return `Error`.
    - Check for duplicate variable bindings between `Γ_i` and the current `Γ_{args}`. If overlap, return `Error`.
    - Update `Γ_{args} = Γ_{args} ∪ Γ_i`.
- **Output:** Return the accumulated context `Γ_{args}`.

---

### `DeriveArgs`
`DeriveArgs(S)` → `Γ_{args}` or `Error`

- **Input:** An initial session type `S`.
- **Logic:** This function encapsulates the framework-specific logic for determining the expected arguments of a handler based on its initial state `S`.
    - Example: If `S = \&p:\{label(Data).S'\}`, `DeriveArgs(S)` might return `{ payload : Data, ctx : SessionCtx, state : ActorState }`.
    - Example: If `S = \text{end}`, `DeriveArgs(S)` might return `{ ctx : SessionCtx, state : ActorState }`.
- **Output:** The context `Γ_{args}` containing the derived argument names and types, or `Error` if `S` doesn't represent a valid starting state for a handler.
- _**Note:** This helper needs a precise definition based on your framework's semantics._

---

### `CheckSpecArgs`
`CheckSpecArgs(\widetilde{T_{spec}}, \Gamma_{args})` → `Bool`

- **Input:** Spec types `\widetilde{T_{spec}} = (T_{s1}, ..., T_{sn})`, derived bindings `Γ_{args}` (containing types `A_1, ..., A_n`).
- **Check Arity:** Verify `length(\widetilde{T_{spec}})` equals the number of bindings in `Γ_{args}`. If not, return `false`.
- **Check Types:** For each `i`, check if `A_i <: T_{si}` using the subtyping relation. If any check fails, return `false`.
- **Output:** Return `true`.

---
### `CheckSpecReturn`
`CheckSpecReturn(T_{spec\_ret}, T'_{ret})` → `Bool`

- **Input:** Spec return type `T_{spec\_ret}`, Actual computed return type `T'_{ret}`.
- **Check Type:** Check if `T'_{ret} <: T_{spec\_ret}` using the subtyping relation.
- **Output:** Return `true` or `false`.