## Pattern Matching Judgement
$$
\Large \vdash p : A ⟹ Γ'
$$

Checks if pattern `p` is valid against type `A` and produces variable bindings `Γ'`.

- **Variable:** Binds `x` to the expected type `A`.
$$  \frac{}{ \vdash x : A \implies \{x:A\} } \quad (\text{Pat-Var}) $$


- **Wildcard:** Matches anything, binds nothing.
$$  \frac{}{ \vdash \_ : A \implies \cdot } \quad (\text{Pat-Wild}) $$


- **Literal:** Matches if the literal `b` has the expected type `A`. Binds nothing.
$$  \frac{\Gamma \vdash b : C \quad C <: A}{ \vdash b : A \implies \cdot } \quad (\text{Pat-Lit}) $$


- **Tuple:** Matches a tuple type, recursively checks elements.
$$  \frac{ A = \text{Tuple}[A_1, \dots, A_n] \quad \forall i \in 1..n \quad \vdash p_i : A_i \implies \Gamma_i }{ \vdash \{p_1, \dots, p_n\} : A \implies \bigcup_{i=1..n} \Gamma_i } \quad (\text{Pat-Tuple}) $$
_(Requires disjoint variable bindings in the `Γᵢ`)_


- **List Cons:** Matches a `List[A]` type, recursively checks head/tail.
$$  \frac{ \vdash p_1 : A \implies \Gamma_1 \quad \vdash p_2 : \text{List}[A] \implies \Gamma_2 }{ \vdash [p_1 | p_2] : \text{List}[A] \implies \Gamma_1 \cup \Gamma_2 } \quad (\text{Pat-Cons}) $$
_(Requires disjoint variable bindings)_


- **Empty List:** Matches an empty `List[A]`. Binds nothing.
$$  \frac{}{ \vdash [] : \text{List}[A] \implies \cdot } \quad (\text{Pat-EmptyList}) $$


- **Record/Map Pattern:** Matches a record type `A`. Pattern keys `k_i` (literal atoms) must exist in `A`. Recursively checks sub-patterns `p_i`.

$$  \frac{ A = \{ k'_j : B'_j \}_{j \in J} \quad \{k_i\}_{i \in I} \subseteq \{k'_j\}_{j \in J} \quad \forall i \in I.\ \exists j \in J.\ (k_i = k'_j \land \vdash p_i : B'_j \implies \Gamma_i) }{ \vdash \%\{ (k_i \Rightarrow p_i) \}_{i \in I} : A \implies \bigcup_{i \in I} \Gamma_i } \quad (\text{Pat-Record}) $$
_(Requires literal atom keys `k_i` in pattern, disjoint bindings)_