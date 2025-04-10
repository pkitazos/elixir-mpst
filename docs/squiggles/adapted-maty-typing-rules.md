# Elixir-Maty Type System

This document outlines the typing rules adapted from the Maty framework which are used in by our typechecker implementation.

---

## Environments

* **`Γ` (Gamma - Local Type Environment):** Maps variables to types within a local scope.
```math
Γ ::= · | Γ, x:A
```

* **`Δ` (Delta - Module Handler Environment):** Maps handler labels (atoms) to their initial session types within a module scope. Constructed from `@st` annotations.
```math
Δ ::= \{ h_1 \mapsto S_1, \dots, h_n \mapsto S_n \}
```

```math
\Phi ::= \cdot \mid \Phi, f : (A_1, \dots, A_n) \rightarrow B \quad \text{or simply} \quad \Phi ::= \cdot \mid \Phi, f_n : \text{FuncType}
```


---

## Judgements

* **Pure Judgement:** In context $\Gamma$, expression $e$ has type $A$. Used for session-unaware code.
```math
\Large \Phi;\ \Gamma \vdash e : A
```
* **Session-Aware Judgement:** Under module handler environment $\Delta$ and local environment $\Gamma$, starting in session state $S_1$, expression $e$ evaluates to type $B$ and ends in session state $S_2$.
```math
\Large \Phi; \ \Delta; \ \Gamma  \mid S_1 \rhd e : B \lhd S_2
```

---

## Type Syntax

```math
\begin{align}
\text{Base Types} \quad C &::= \mathbb{1} \mid \text{Nil} \mid \text{Bool} \mid \text{Atom} \mid \text{Number} \mid \text{Binary} \mid \text{PID} \mid \text{Ref} \mid \text{Date} \mid \dots \\
\text{Map Key Types} \quad K &::= \text{Atom} \mid \text{Number} \mid \text{Binary} \mid \dots \quad \text{(rest allowed by our grammar)} \\
\text{Maty Types} \quad M &::= \text{Role} \mid \text{SessionCtx} \mid \text{SessionID} \mid \text{InitToken} \mid \text{ActorState} \mid \text{APState} \\
\text{Types} \quad A, B &::= C \mid M \\ & \mid \text{List}[A] \mid \text{Tuple}[A_1, \dots, A_n] \mid \text{Map}[K, A] \\ & \mid A \rightarrow B \\ & \mid \text{Handler}(S) \quad \text{(Type of handler label expecting state S)} \\ & \mid \bot \quad \text{(Bottom type)}
\end{align}
```
*(Note: Session types `S` are used to parameterise `Handler(S)` but are not first-class types themselves).*

---

## Maty Type Definitions (Formal Structure)

```math
\begin{align}
% Component Types used in Maty structures
\text{SessionID} &::= \text{Ref} \\
\text{InitToken} &::= \text{Ref} \\
\text{Role} &::= \text{Atom} \\
\text{HandlerLabel} &::= \text{Atom} \\
\text{Label} &::= \text{Atom} \\
\text{Function} &::= A \rightarrow B \quad \\
\text{Any} &::= \top \quad && \text{(Top type)} \\
\text{Queue}[A] & \quad && \text{(Abstract Queue type constructor)} \\
\\
% Composite Maty Types
\text{Session} &::= \{ \\ & \quad \texttt{id} : \text{SessionID}, \\ & \quad \texttt{handlers} : \text{Map}[\text{Role}, \text{Tuple}[\text{Function}, \text{Role}]], \\ & \quad \texttt{participants} : \text{Map}[\text{Role}, \text{PID}], \\ & \quad \texttt{local\_state} : \text{Any} \\ & \} \\
\text{SessionCtx} &::= \text{Tuple}[\text{Session}, \text{Role}] \\
\text{ActorState} &::= \{ \\ & \quad \texttt{sessions} : \text{Map}[\text{SessionID}, \text{Session}], \\ & \quad \texttt{callbacks} : \text{Map}[\text{InitToken}, \text{Tuple}[\text{Role}, \text{Function}]] \\ & \} \\
\text{APState} &::= \{ \\ & \quad \texttt{participants} : \text{Map}[\text{Role}, \text{Queue}[\text{Tuple}[\text{PID}, \text{InitToken}]]] \\ & \} \\
\\
% Runtime Tuple Value Types (related to Maty)
\text{SuspendVal} &::= \text{Tuple}[:\texttt{suspend}, \text{HandlerLabel}, \text{ActorState}] \\
\text{DoneVal} &::= \text{Tuple}[:\texttt{done}, \mathbb{1}, \text{ActorState}]
\end{align}
```

---

## Session Type Syntax

```math
\begin{align}
S &::= \mathtt{end} \quad &&(\text{Protocol finished successfully}) \\
& \mid \emptyset \quad &&(\text{Suspended - no session continuation this path}) \\
& \mid \oplus p:\{l_i(A_i).S_i\}_{i \in I} \quad &&(\text{Internal Choice: Send } l_j(A_j) \text{ to } p\text{, continue as } S_j) \\
& \mid \& p:\{l_i(A_i).S_i\}_{i \in I} \quad &&(\text{External Choice: Expect } l_j(A_j) \text{ from } p\text{, continue as } S_j) \\
& \mid h \quad &&(\text{Continue protocol at handler } h\text{'s session type})
\end{align}
```
*(Note: Continuation via handler labels `h` requires lookup in `Δ`.)*

---

## Join Operator (`⊔`) for Branch Outcomes

Used in `T-Case`. Combines outcomes `(T, S')` where `T` can be `⊥` and `S'` can be `Ø`.
* `(⊥, Ø) ⊔ o = o`
* `o ⊔ (⊥, Ø) = o`
* `(T₁, S₁) ⊔ (T₂, S₂)` = `(T₁, S₁)` if `T₁ = T₂` and `S₁ = S₂`.
* `(T₁, S₁) ⊔ (T₂, S₂)` = `TypeError` otherwise (if `T₁, T₂ ≠ ⊥` and `S₁, S₂ ≠ Ø`).

*(This allows mixing suspension `(⊥, Ø)` with a single concrete outcome `(T, S)`, yielding `(T, S)`. It requires convergence otherwise.)*

---

## Pure Typing Rules
```math
\Large \Phi;\ \Gamma \vdash e : A
```

**(T-Var)**
```math
\frac{ x:A \in \Gamma }{ \Gamma \vdash x:A } \quad (\text{T-Var})
```


**(T-Literal)**
```math
\frac{ c \text{ has base type } C }{ \Gamma \vdash c:C } \quad (\text{T-Literal})
```


**(T-Let-Pure)**
```math
\frac{ \Gamma \vdash M : A \quad \quad \Gamma, x:A \vdash N : B }{ \Gamma \vdash (x = M; N) : B } \quad (\text{T-Let-Pure})
```


**(T-App-Pure)**
```math
\frac{ \Gamma \vdash V : A \rightarrow B \quad \quad \Gamma \vdash W : A }{ \Gamma \vdash V\;W : B } \quad (\text{T-App-Pure})
```


**(T-Case-Pure)**
```math
\frac{ \begin{array}{c} \Gamma \vdash E : A \\ \forall i \in I.\ ( \vdash p_i : A \implies \Gamma_i \;\land\; \Gamma, \Gamma_i \vdash N_i : B ) \end{array} }{ \Gamma \vdash (\text{case } E \text{ do } \{p_i \rightarrow N_i\}_{i \in I} \text{ end}) : B } \quad (\text{T-Case-Pure})
```
*(Requires all branches to converge to the same type `B`)*

---

## Session-Aware Typing Rules

```math
\Large \Phi; \ \Delta; \ \Gamma  \mid S_1 \rhd e : B \lhd S_2
```
  
**(T-HandlerLabel)** Assigns `Handler(S)` type to a handler label `h`.
```math
\frac{ \Delta(h) = S }{ \Delta; \Gamma \vdash h : \text{Handler}(S) } \quad (\text{T-HandlerLabel})
```
*(Assumes `WF-Handler` check passed for `h`)*
  
  
**(T-Done)** Types the `{:done, ...}` tuple.
```math
\frac{ \Gamma \vdash e_{state} : \text{ActorState} }{ \Delta; \Gamma | S \rhd \{:\texttt{done}, \mathbb{1}, e_{state}\} : \text{DoneVal} \lhd \mathtt{end} } \quad (\text{T-Done})
```
*(Assumes state `S` allows termination)*
  
  
**(T-SuspendExpr)** Types the `{:suspend, ...}` tuple.
```math
\frac{ \Delta(h) = S_h \quad \quad \Gamma \vdash e_{state} : \text{ActorState} }{ \Delta; \Gamma | S_h \rhd \{:\texttt{suspend}, h, e_{state}\} : \bot \lhd \emptyset } \quad (\text{T-SuspendExpr})
```
*(Requires current state to be the state `S_h` expected by handler `h`)*
  
  
**(T-Let)** Session-aware sequence.
```math
\frac{ \Delta; \Gamma \mid S_1 \rhd M : A \lhd S_1 \quad \quad \Delta; \Gamma, x:A \mid S_1 \rhd N : B \lhd S_2 }{ \Delta; \Gamma \mid S_1 \rhd (x = M; N) : B \lhd S_2 } \quad (\text{T-Let})
```
  
  
**(T-App)** Session-aware application (assumed session-neutral itself).
```math
\frac{ \Gamma \vdash V : A \rightarrow B \quad \quad \Gamma \vdash W : A }{ \Delta; \Gamma \mid S \rhd V\;W : B \lhd S } \quad (\text{T-App})
```
  
  
**(T-Send)** Session-aware send.
```math
\frac{ \Gamma \vdash \text{s}\!:\!\text{SessionCtx} \quad \Gamma \vdash \text{p}\!:\!\text{Role} \quad \Gamma \vdash \text{l}\!:\!\text{Label} \quad \Gamma \vdash V\!:\!A_j \quad l = l_j \quad j \in I }{ \Delta; \Gamma \mid \text{p} \oplus \{ l_i(A_i).S_i \}_{i \in I} \rhd \texttt{maty\_send}(\text{s}, \text{p}, \{\text{l}, V\}) : \mathbb{1} \lhd S_j } \quad (\text{T-Send})
```
  
  
**(T-Case)** Session-aware case, uses join `⊔`.
```math
\frac{ \begin{array}{c} \Gamma \vdash E : A \\ \forall i \in I.\ ( \vdash p_i : A \implies \Gamma_i \;\land\; \Delta; \Gamma, \Gamma_i \mid S_0 \rhd N_i : B_i \lhd S'_{i} ) \\ (B, S_2) = \bigsqcup_{i \in I} (B_i, S'_{i}) \end{array} }{ \Delta; \Gamma \mid S_0 \rhd (\text{case } E \text{ do } \{p_i \rightarrow N_i\}_{i \in I} \text{ end}) : B \lhd S_2 } \quad (\text{T-Case})
```

---

## Well-Formedness Rules

**(WF-Handler)** Checks handler definitions (`H = @handler h F`).
```math
\frac{
  \begin{array}{l}
  % 1. Annotation & Function Link
  \Delta(h) = S_{initial} \\
  % 2. Derive Expected Args & Bindings from Session State
  \text{DeriveArgs}(S_{initial}) = \Gamma_{args} \\
  % 3. Check Spec Args (compare spec types with types derived from session state)
  \text{CheckSpecArgs}(\text{ArgsSpec}(F), \text{TypesOf}(\Gamma_{args})) \\
  % 4. Check Body using session judgement, starting context includes global functions Φ
  \Delta; \Phi, \Gamma_{args} | S_{initial} \rhd \text{Body}(F) : T'_{ret} \lhd S'_{final} \\
  % 5. Check Return Type against Spec and Suspend/Done union
  \text{CheckSpecReturn}(\text{ReturnSpec}(F), T'_{ret}) \quad \land \quad T'_{ret} <: (\text{SuspendVal} \sqcup \text{DoneVal}) \\
  % 6. Check Final State is valid termination/suspension
  (S'_{final} = \emptyset \lor S'_{final} = \mathtt{end})
  \end{array}
}{
  % Conclusion: Handler definition H is well-formed (given Δ and Φ)
  \Delta; \Phi \vdash H \text{ ok}
}
\quad (\text{WF-Handler})
```


**(WF-Func)** Checks regular function definitions (`F = [@spec...] def f do t end`).
```math
\frac{
  \begin{array}{l}
  % 1. Bind formal parameters according to patterns and spec types
  \text{BindParams}(\text{Params}(F), \text{ArgsSpec}(F)) = \Gamma_{args} \\
  % 2. Check function body using PURE judgement, starting context includes global functions Φ
  \Phi, \Gamma_{args} \vdash \text{Body}(F) : T'_{ret} \\
  % 3. Check actual return type against spec return type
  \text{CheckSpecReturn}(\text{ReturnSpec}(F), T'_{ret})
  \end{array}
}{
  % Conclusion: Function definition F is well-formed (given Δ and Φ)
  % The judgement could optionally conclude with adding f's type to Φ if built incrementally
  \Delta; \Phi \vdash F \text{ ok}
}
\quad (\text{WF-Func})
```

---
