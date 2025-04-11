---
tags:
  - v1
---

## Binary Operators (`◇`)
**Session-Neutral / Pure**
```math
\frac{
  \Gamma \vdash e_1 : T \quad \Gamma \vdash e_2 : T \quad \text{OpType}(\diamond) = (T \rightarrow T \rightarrow T')
}{
  \Gamma \vdash (e_1 \diamond e_2) : T'
}
\quad (\text{T-Op-Pure})
```

**Session-Aware**
```math
\frac{
  % Check operands purely, ensuring same input type T_in
  \Gamma \vdash e_1 : T \quad \Gamma \vdash e_2 : T \quad \text{OpType}(\diamond) = (T \rightarrow T \rightarrow T')
}{
  % Session state S is unchanged
  \Delta; \Gamma | S \rhd (e_1 \diamond e_2) : T' \lhd S
}
\quad (\text{T-Op})
```

### OpType
```math
\begin{align*}
\text{OpType}(op) &=

\begin{cases}

\text{Number} \rightarrow \text{Number} \rightarrow \text{Number} & \text{if } op \in \{+, -, *, /\} \\

\text{Binary} \rightarrow \text{Binary} \rightarrow \text{Binary} & \text{if } op \in \{\texttt{<>}\} \\

\text{Bool} \rightarrow \text{Bool} \rightarrow \text{Bool} & \text{if } op \in \{\texttt{and}, \texttt{or}\} \\

\text{Number} \rightarrow \text{Number} \rightarrow \text{Bool} & \text{if } op \in \{\lt, \gt, \leq, \geq \} \\

\forall \alpha . \alpha \rightarrow \alpha \rightarrow \text{Bool} & \text{if } op \in \{==, \neq \}

\end{cases}

\end{align*}
```


---

## Not Operator (`not e`)
**Session-Neutral / Pure**
```math
\frac{
  \Gamma \vdash e : \text{Bool}
}{
  \Gamma \vdash (\texttt{not } e) : \text{Bool}
}
\quad (\text{T-Not-Pure})
```

**Session-Aware**
```math
\frac{
  \Gamma \vdash e : \text{Bool}
}{
  \Delta; \Gamma | S \rhd (\texttt{not } e) : \text{Bool} \lhd S
}
\quad (\text{T-Not})
```


---

## List Construction (`[e₁ | e₂]`)

**Session-Neutral / Pure**
```math
\frac{
  \Gamma \vdash e_1 : A \quad \Gamma \vdash e_2 : \text{List}[A]
}{
  \Gamma \vdash [e_1 | e_2] : \text{List}[A]
}
\quad (\text{T-Cons-Pure})
```

**Session-Aware**
```math
\frac{
  \Gamma \vdash e_1 : A \quad \Gamma \vdash e_2 : \text{List}[A]
}{
  \Delta; \Gamma | S \rhd [e_1 | e_2] : \text{List}[A] \lhd S
}
\quad (\text{T-Cons})
```
---

## Tuple Construction (`{e₁, ..., eₙ}`)

**Session-Neutral / Pure**
```math
\frac{
  \forall i \in 1..n \quad \Gamma \vdash e_i : A_i
}{
  \Gamma \vdash \{e_1, \dots, e_n\} : \text{Tuple}[A_1, \dots, A_n]
}
\quad (\text{T-Tuple-Pure})
```

**Session-Aware**
```math
\frac{
  \forall i \in 1..n \quad \Gamma \vdash e_i : A_i
}{
  \Delta; \Gamma | S \rhd \{e_1, \dots, e_n\} : \text{Tuple}[A_1, \dots, A_n] \lhd S
}
\quad (\text{T-Tuple})
```

---
## Map/Record Construction

**Session-Neutral / Pure**

**Records** - For map literals with fixed atom keys (e.g., `%{ :id => ..., :participants => ... }`)

```math
\frac{
  \forall i \in 1..n \quad (\Gamma \vdash e_i : A_i \quad \land \quad k_i = \texttt{:}atom_i)
  % k_i are distinct literal atoms
}{
  \Gamma \vdash \%\{ (k_i \Rightarrow e_i)_{i=1..n} \} : \{ k_1: A_1, \dots, k_n: A_n \}
}
\quad (\text{T-Record-Pure})
```
_(This assigns a specific record type `{ k₁: A₁, ..., kₙ: Aₙ }` which details the type `Aᵢ` for each distinct atom key `kᵢ`)_


**Maps** - For general maps where keys (`wᵢ`) might not be fixed atoms, requires key/value type homogeneity.

```math
\frac{
  \forall i \in I \quad (\Gamma \vdash w_i : K \quad \land \quad \Gamma \vdash e_i : A)
  % Requires inference/check that all keys have type K, values have type A
}{
  \Gamma \vdash \%\{ (w_i \Rightarrow e_i)_{i \in I} \} : \text{Map}[K, A]
}
\quad (\text{T-Map-Pure})
```


**Session-Aware**

**Records**
```math
\frac{
  \forall i \in 1..n \quad (\Gamma \vdash e_i : A_i \quad \land \quad k_i = \texttt{:}atom_i)
}{
  \Delta; \Gamma | S \rhd \%\{ (k_i \Rightarrow e_i)_{i=1..n} \} : \{ k_1: A_1, \dots, k_n: A_n \} \lhd S
}
\quad (\text{T-Record})
```


**Maps**
```math
\frac{
  \forall i \in I \quad (\Gamma \vdash w_i : K \quad \land \quad \Gamma \vdash e_i : A)
  % Requires inference/check that all keys have type K, values have type A
}{
  \Delta; \Gamma | S \rhd \%\{ (w_i \Rightarrow e_i)_{i \in I} \} : \text{Map}[K, A] \lhd S
}
\quad (\text{T-Map})
```