## Module Well-Formedness

$$
\frac{
  \begin{array}{l}
  % 1. Build Handler Env from annotations (check for duplicates)
  \Delta = \text{BuildHandlerEnv}(\widetilde{K}) \\
  % 2. Build Global Function Env (check for duplicate defs/specs)
  \Phi = \text{BuildFuncEnv}(\widetilde{H}, \widetilde{F}) \\
  % 3. Check all Function Definitions (incl. those used in Handlers) using WF-Func
  \forall F \text{ in definitions from } (\widetilde{H} \cup \widetilde{F}) . \quad \Delta; \Phi \vdash F \text{ ok} \\
  % 4. Check all Handler Definitions using WF-Handler
  \forall H = (\texttt{@handler } h \ F) \in \widetilde{H} . \quad \Delta; \Phi \vdash H \text{ ok}
  \end{array}
}{
  % Conclusion: Module M is well-formed
  \vdash M \text{ ok}
}
\quad (\text{WF-Module})
$$


1. **`Δ = BuildHandlerEnv(K~)`:** Constructs the handler environment `Δ` from all `@st` annotations. Fails if duplicate handler labels `h` are defined.

2. **`Φ = BuildFuncEnv(H~, F~)`:** Constructs the global function signature environment `Φ` from all function definitions (`def`) and their `@spec`s. 
	- Does not fail if duplicate function definitions/signatures exist (for the same arity).

3. **`∀ F ... . Δ; Φ ⊢ F ok`:** Applies the `WF-Func` rule to _every_ function definition `F` found in the module (both standalone functions and those implementing handlers). `Γ` (local context) is initialised to `Φ`.

4. **`∀ H ... . Δ; Φ ⊢ H ok`:** Applies the `WF-Handler` rule to _every_ handler definition `H` (`@handler h F`). `Γ` is initialised to `Φ`.