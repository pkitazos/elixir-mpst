## Subtyping Relation
```math
\Large A <: B
```

This defines when type `A` is considered a subtype of `B`. We assume structural subtyping.

- **Reflexivity:** A type is a subtype of itself.
```math
\frac{}{A <: A} \quad (\text{Sub-Refl})
```


- **Transitivity:** Subtyping is transitive.
```math
\frac{A <: B \quad B <: C}{A <: C} \quad (\text{Sub-Trans})
```


- **Top Type (`⊤` / `Any`):** Any type is a subtype of `⊤`.
```math
\frac{}{A <: \top} \quad (\text{Sub-Top})
```


- **Bottom Type (`⊥`):** Bottom is a subtype of any type.
```math
\frac{}{\bot <: A} \quad (\text{Sub-Bot})
```


- **Functions (`→`):** Contravariant in arguments, covariant in results.
```math
\frac{A_2 <: A_1 \quad B_1 <: B_2}{(A_1 \rightarrow B_1) <: (A_2 \rightarrow B_2)} \quad (\text{Sub-Func})
```


- **Tuples:** Covariant in elements.
```math
\frac{\forall i \in 1..n \quad A_i <: B_i}{\text{Tuple}[A_1, \dots, A_n] <: \text{Tuple}[B_1, \dots, B_n]} \quad (\text{Sub-Tuple})
```


- **Lists:** Covariant in element type.
```math
\frac{A <: B}{\text{List}[A] <: \text{List}[B]} \quad (\text{Sub-List})
```


- **Maps:** Invariant in keys, covariant in values (common choice).
```math
\frac{K_1 = K_2 \quad A <: B}{\text{Map}[K_1, A] <: \text{Map}[K_2, B]} \quad (\text{Sub-Map})
```


- **Records:** Width and depth subtyping (subtype has _at least_ the fields of supertype, with compatible types).
```math
\frac{ \{k'_j\}_{j \in J} \subseteq \{k_i\}_{i \in I} \quad \forall j \in J.\ \exists i \in I.\ (k_i = k'_j \land A_i <: B'_j) }{ \{ k_i : A_i \}_{i \in I} <: \{ k'_j : B'_j \}_{j \in J} } \quad (\text{Sub-Record})
```
_(Note: Still need rules for base types, Maty types usually `A <: A` and `A <: ⊤`)_

