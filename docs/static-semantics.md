---
tags:
  - v5
---
# Elixir Syntax

```math
\begin{align}
&\text{Variable} &x \\
&\text{Function} &f \\
&\text{Role} &\color{#6d28d9}{\mathsf{q}} \color{#4A4943},\ \color{#6d28d9}{\mathsf{r}} \\
&\text{Label} &l \\
&\text{Handler Name} &h \\
\\
&\text{Module} &M &::= \texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}\\
&\text{Session Annotation} &K &::= \texttt{@st} \ \{h, S\} \\
&\text{Init Handler} &H_{\text{I}} &::= \texttt{init\_handler } h,\ \{ p_1 :: A_{1},\ \dots,\ p_n :: A_{n}\},\ x \texttt{ do } e \texttt{ end} \\
&\text{Message Handler} &H_{\text{M}} &::= \texttt{handler } h,\ \color{#6d28d9}{\mathsf{q}}\ \color{#4A4943}, \{ l, p :: A\},\ x \texttt{ do } e \texttt{ end} \\
&\text{Function} &F &::= \texttt{@spec} \ f(\widetilde{A}) :: B \ \texttt{def} \ f(p_1,\ \dots,\ p_n) \  \texttt{do} \ e \ \texttt{end}\\
\\
&\text{Basic Values} &b &::= \text{atom} \mid \text{nil} \mid \text{boolean} \mid \mathsf{Number} \mid \mathsf{Binary} \mid \text{date} \mid \text{PID} \mid \text{Ref} \mid [] \\
&\text{Values} &v &::= b \mid x \mid h \mid [v_1 \mid v_2] \mid \{v_1, \ \dots, \ v_n\} \mid \texttt{\%}\{ (v_i \ \texttt{=>} \ v_i)_{i \in I} \} \\
\\
&\text{Patterns} &p &::= v \\
&&&\mid \_ \\
&&&\mid [p_{1} \mid p_{2}] \mid [] \\
&&&\mid \{ p_{1}, \dots, p_{n} \} \mid \{\} \\
&&&\mid \%\{k_{i} \Rightarrow p_{i}, \dots \} \\
\\
&\text{Expressions} &e &::= v \\
&&&\mid v_1 \ \diamond \ v_2 \mid \texttt{not} \ v \\
&&&\mid p=e_1;\ e_2 \\
&&&\mid f(v_1, \ \dots, \ v_n) \\
&&&\mid \texttt{case} \ v \ \texttt{do} \ (p_i \rightarrow e_i)_{i \in I} \ \texttt{end} \\
&&&\mid [] \mid \ \{\} \mid \ \%\{\} \mid [v_{1} \mid v_{2}] \mid \{ v_{1}, \dots, v_{n} \} \mid \%\{k_{i} \Rightarrow v_{i}   \} \\
&&&\mid \texttt{maty\_register}(v_{1}, \color{#6d28d9}{\mathsf{q}}\ \color{#4A4943}, h, v_{2}) \\
&&&\mid \texttt{maty\_send}(\color{#6d28d9}{\mathsf{q}}\ \color{#4A4943},\ \{l, v\}) \\
&&&\mid \texttt{maty\_suspend}(h, v) \\
&&&\mid \texttt{maty\_done}(v) \\
\\
&\text{Binary Operators} &\diamond &::=  \texttt{<} \mid \texttt{>} \mid \texttt{<=}  \mid \texttt{>=} \mid \texttt{==} \mid \texttt{!=} \mid \texttt{+} \mid \texttt{-} \mid \texttt{*} \mid \texttt{/} \mid \texttt{<>} \mid \texttt{and} \mid  \texttt{or}
\end{align}
```


# Types & Session Types

## Types

```math
\begin{align}
&\text{Optional Types} &T &::= A \mid \bot_{T} \\
&\text{Base Types} &C &::= \mathsf{Atom} \mid \mathsf{Nil} \mid \mathsf{Bool} \mid \mathsf{Number} \mid \mathsf{Binary} \mid \mathsf{Date} \mid \mathsf{PID} \mid \mathsf{Ref} \\
&\text{Types} &A, B &::= C \\ 
&&&\mid \mathsf{List}[A] \mid \mathsf{Tuple}[\widetilde{A}] \mid \mathsf{Map}[C, A] \\
&&&\mid (\widetilde{A}) \rightarrow B \\ 
&&&\mid \mathsf{Handler}(S^?) \\ 
&&&\mid \mathsf{InitHandler}(S) \\ 
\end{align}
```

## Session Types

```math
\begin{align}
Q &::= S \mid \bot_{S} \\
\\
S &::= \mathsf{end} \mid S^! \mid S^? \mid h \\
\\
S^! &::= \oplus p:\{l_i(A_i).S_i\}_{i \in I} &&(\text{Internal Choice: Send } l_j(A_j) \text{ to } p\text{, continue as } S_j) \\
S^? &::= \& p:\{l_i(A_i).S_i\}_{i \in I} &&(\text{External Choice: Expect } l_j(A_j) \text{ from } p\text{, continue as } S_j) \\
\end{align}
```


```math
\begin{align}
&\text{Optional types}& T &::= A \mid \bot_{T} \\
&\text{Base types}& C &::= \mathsf{Atom} \mid \mathsf{Nil} \mid \mathsf{Bool} \mid \mathsf{Number}\\
&&&\mid \mathsf{Binary} \mid \mathsf{Date} \mid \mathsf{PID} \mid \mathsf{Ref} \\
&\text{Types}& A, B &::= C \\ 
&&&\mid \mathsf{List}[A] \mid \mathsf{Tuple}[\widetilde{A}] \mid \mathsf{Map}[C, A] \\
&&&\mid (\widetilde{A}) \rightarrow B \\ 
&&&\mid \mathsf{Handler}(S^?) \\ 
&&&\mid \mathsf{InitHandler}(S) \\ 
\\ \\
&\text{Optional session types}&Q &::= S \mid \bot_{S} \\
&\text{Session types}&S &::= \mathsf{end} \mid S^! \mid S^? \mid h \\
&\text{Output session types}&S^! &::= \oplus p:\{l_i(A_i).S_i\}_{i \in I} \\
&\text{Input session types}&S^? &::= \& p:\{l_i(A_i).S_i\}_{i \in I}\\
\end{align}
```

## Join Operator (⊔) for Branch Outcomes

>[!note] note for me
> explicitly mention that the $\bot$ operator is commutative and associative


**For Session Types** ($\bot = \bot_{S}$)
```math
\begin{align}
	Q \sqcup Q &= Q \\
	\bot \sqcup Q &= Q \\
	Q \sqcup \bot &= Q \\
	\bot \sqcup \bot &= \bot \\
\end{align}
```

**For Types** ($\bot = \bot_{T}$)
```math
\begin{align}
	T \sqcup T &= T \\
	\bot \sqcup T &= T \\
	T \sqcup \bot &= T \\
	\bot \sqcup \bot &= \bot \\
\end{align}
```

**For Session Type - Type Pairs**
```math
(Q_a, \ T_a) \ \sqcup \ (Q_b, \ T_b) = (Q_a \sqcup Q_b, \ T_a \sqcup T_b)
```

>[!note] note for me
>also make sure to mention how the $\bigsqcup$ rule works


---

# Meta-level Module Functions

```math
\begin{align}
&\text{name}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &m \\

&\text{session\_types}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &\widetilde{K} \\

&\text{init\_handlers}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &\widetilde{H}_{\text{I}} \\

&\text{handlers}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &\widetilde{H}_{\text{M}} \\

&\text{functions}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &\widetilde{F}
\end{align}
```


```mathmath
\begin{align}
&\text{make\_func\_env}(M) &= \quad &\{ (f, n) \mapsto (A_{1}, …, A_{n}) \to B \mid \\
&& \quad &\texttt{@spec } f(A_{1}, \dots, A_{n}) :: B  \\
&& \quad & \texttt{def } f(p_{1}, \dots, p_{n}) \texttt{ do }  e \texttt{ end } \in \text{functions}(M) \}
\\ \\
&\text{make\_msg\_handler\_env}(M) &= \quad &\{ h \mapsto S \mid
\texttt{handler } h \ \dots \text{ end} \in \text{handlers}(M), \\
&& \quad &\texttt{@st } \{ h_{K},\ S \} \in \text{session\_types}(M),\ h=h_{K} \} \\ \\
&\text{make\_init\_handler\_env}(M) &= \quad &\{ h \mapsto S \mid \texttt{init\_handler } h \ \dots \text{ end} \in \text{init\_handlers}(M), \\
&& \quad &\texttt{@st } \{ h_{K},\ S \} \in \text{session\_types}(M),\ h=h_{K} \} 
\end{align}
```

# Environments

#### Variable Binding Environment

```math
\Large \Gamma = \cdot \mid \Gamma, x:A 
```

#### Module Function Type Environment

```math
\Large \Psi = \{ (f, n) \mapsto (A_1, \dots, A_n) \to B \}
```

#### Module Handler Environment

```math
\Large
\Delta = \{ h \mapsto S \} \\
```



# Pattern Matching

> [!note] note for me
> framebox around the typing judgements

```math
\Large \vdash p : A \implies \Gamma'
```

```math
\begin{align*}
&\frac{
}{
  \vdash x : A \implies \{x:A\}
} && (\text{Pat-Var}) \\ \\
&\frac{
}{
  \vdash \_ : A \implies \cdot
} && (\text{Pat-Wild}) \\ \\
&\frac{
  \Gamma \vdash v : A \quad \text{(v is a literal value)}
}{
  \vdash v : A \implies \cdot
} && (\text{Pat-Value}) \\ \\
&\frac{
  \begin{array}{c}
    \vdash p_1 : A \implies \Gamma_1 \\
    \vdash p_2 : \mathsf{List}[A] \implies \Gamma_2 \\
    \text{dom}(\Gamma_1) \cap \text{dom}(\Gamma_2) = \emptyset
  \end{array}
}{
  \vdash [p_1 \mid p_2] : \mathsf{List}[A] \implies \Gamma_1 \cup \Gamma_2
} && (\text{Pat-Cons}) \\ \\
&\frac{
  B = \mathsf{List}[A'] \quad \text{(for some A')}
}{
  \vdash [] : B \implies \cdot
} && (\text{Pat-EmptyList}) \\ \\
&\frac{
  \begin{array}{c}
    B = \mathsf{Tuple}[A_1, \dots, A_n] \\
    (\vdash p_i : A_i \implies \Gamma_i)_{i=1..n} \\
    \forall i \neq j \in \{1..n\} . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset
  \end{array}
}{
   \vdash \{p_1, \dots, p_n\} : B \implies \bigcup_{i=1..n} \Gamma_i
} && (\text{Pat-Tuple}) \\ \\
&\frac{
   A = \mathsf{Tuple}[]
}{
   \vdash \{\} : A \implies \cdot
} && (\text{Pat-EmptyTuple}) \\ \\
&\frac{
  \begin{array}{c}
    B = \mathsf{Map}[C, A] \\
    (\Gamma \vdash k_i : C)_{i \in I} \\
    (\vdash p_i : A \implies \Gamma_i)_{i \in I} \\
    \forall i \neq j \in I . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset
  \end{array}
}{
  \vdash \%\{ (k_i \Rightarrow p_i) \}_{i \in I} : B \implies \bigcup_{i \in I} \Gamma_i
} && (\text{Pat-Map}) \\ \\
&\frac{
   A = \mathsf{Map}[C, A'] \quad \text{(for some C, A')}
}{
   \vdash \%\{\} : A \implies \cdot
} && (\text{Pat-EmptyMap})
\end{align*}
```



# Value Typing

```math
\Large \Psi;\ \Delta;\ \Gamma \vdash v : A
```


```math
\begin{align*}
&\frac{
  b \text{ has base type } C
}{
  \Psi; \Delta; \Gamma \vdash b : C
} && (\text{Val-BaseLit}) \\ \\
&\frac{
      \Psi; \Delta; \Gamma \vdash v_1 : A \quad \Psi; \Delta; \Gamma \vdash v_2 : \mathsf{List}[A]
}{
      \Psi; \Delta; \Gamma \vdash [v_1 | v_2] : \mathsf{List}[A]
} \quad && (\text{Val-Cons})
 \\ \\
&\frac{
}{
  \Psi; \Delta; \Gamma \vdash [] : \mathsf{List}[\mathsf{Nil}] % Default type
} && (\text{Val-EmptyList}) \\ \\
&\frac{
  (\Psi; \Delta; \Gamma \vdash v_i : A_i)_{i=1..n}
}{
  \Psi; \Delta; \Gamma \vdash \{v_1, \dots, v_n\} : \mathsf{Tuple}[A_1, \dots, A_n]
} && (\text{Val-Tuple}) \\ \\
&\frac{
}{
  \Psi; \Delta; \Gamma \vdash \{\} : \mathsf{Tuple}[]
} && (\text{Val-EmptyTuple}) \\ \\
&\frac{
 (\Psi; \Delta; \Gamma \vdash k_j : C)_{j \in J} \quad (\Psi; \Delta; \Gamma \vdash v_j : A)_{j \in J}
}{
 \Psi; \Delta; \Gamma \vdash \%\{ (k_j \Rightarrow v_j)_{j \in J} \} : \mathsf{Map}[C, A]
} && (\text{Val-Map}) \\ \\
&\frac{
}{
  \Psi; \Delta; \Gamma \vdash \%\{\} : \mathsf{Map}[\mathsf{Atom}, \mathsf{Nil}] % Example default type
} && (\text{Val-EmptyMap})
\end{align*}
```


```math
\begin{align}
&\frac{
	x:A \in \Gamma
}{
	\Psi;\ \Delta;\ \Gamma \vdash x:A
}\quad &(\text{TV-Var})
\\ \\
&\frac{
    h \mapsto S \in \Delta_M \quad S=S^?
}{
    \Psi;\ \Delta;\ \Gamma \vdash h : \mathsf{Handler}(S^{?})
} \quad &(\text{TV-MsgHandler})
\\ \\
&\frac{  \\
    h \mapsto S \in \Delta_I \\
}{ \\
    \Psi;\ \Delta;\ \Gamma \vdash h : \mathsf{InitHandler}(S) \\
} \quad &(\text{TV-InitHandler})
\end{align}
```



# Computation Typing

```math
\Large \Psi; \ \Delta; \ \Gamma \mid Q_1 \rhd e : T \lhd Q_2
```

### Pure Computations



```math
\frac{
    \Psi;\ \Delta;\ \Gamma \vdash v : A
}{
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd v: A \lhd Q
}  \quad (\text{T-Pure})
```

```math
    \frac{
      \Psi;\ \Delta;\ \Gamma \vdash v : \mathsf{Bool}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd (\texttt{not } v) : \mathsf{Bool} \lhd Q
    }  \quad (\text{T-Not})
```

```math
    \frac{
      \begin{array}{c}
        \Psi;\ \Delta;\ \Gamma \vdash v_1 : A \\
        \Psi;\ \Delta;\ \Gamma \vdash v_2 : B \\
        \text{OpTypeRel}(\diamond, A, B, C)
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd v_1 \diamond v_2 : C \lhd Q
    } \quad (\text{T-Op})
```

```math
\begin{gathered}
\text{OpTypeRel}(\diamond, A, B, C) \\
\iff
\\
\begin{array}{l}
 (\diamond \in \{+, -, *, /\} \land A = \mathsf{Number} \land B = \mathsf{Number} \land C = \mathsf{Number}) \quad \lor \\
 (\diamond = \texttt{<>} \land A = \mathsf{Binary} \land B = \mathsf{Binary} \land C = \mathsf{Binary}) \quad \lor \\
 (\diamond \in \{\texttt{and}, \texttt{or}\} \land A = \mathsf{Bool} \land B = \mathsf{Bool} \land C = \mathsf{Bool}) \quad \lor \\
 (\diamond \in \{\lt, \gt, \leq, \geq \} \land A = \mathsf{Number} \land B = \mathsf{Number} \land C = \mathsf{Bool}) \quad \lor \\
 (\diamond \in \{==, \neq \} \land A = B \land C = \mathsf{Bool})
\end{array}
\end{gathered}
```



```math
    \frac{
      \begin{array}{c}
        (\Psi;\ \Delta;\ \Gamma \vdash v_i : A_i)_{i\in1..n} \\
        \Psi(f, n) = (A_{1},\dots,A_{n}) \to B
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd f(v_{1}, \dots,v_{n}) : B \lhd Q
    } \quad (\text{T-App})
```


```math
    \frac{
    \begin{array}{c}
       \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd e_1 : A \lhd Q_2 \\
       \vdash p : A \implies \Gamma' \\
       \Psi;\ \Delta;\ \Gamma, \Gamma' \mid Q_2 \rhd e_2 : T \lhd Q_3
    \end{array}
    }{
       \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd p = e_1; e_2 : T \lhd Q_3
    } \qquad (\text{T-Let})
```


```math
    \frac{
      \begin{array}{c}
        \Psi;\ \Delta;\ \Gamma \vdash v : A \\
        \forall i \in I.\ (
          \vdash p_i : A \implies \Gamma_i
          \quad \land \quad
          \Psi;\ \Delta;\ \Gamma, \Gamma_i \mid Q_1 \rhd e_i : T_i \lhd Q'_{i}
        ) \\
        (T, Q_2) = \bigsqcup_{i \in I} (T_i, Q'_{i})
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd \texttt{case } v \texttt{ do } (p_i \to e_i)_{i \in I} \text{ end} : T \lhd Q_2
    } \qquad (\text{T-Case})
```


```math
\frac{
  \Psi;\ \Delta;\ \Gamma \vdash v : \mathsf{ActorState}(A)
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{getState}(v) : A \lhd Q
} \qquad (\text{T-Get})
```


```math
\frac{
  \begin{array}{c}
    \Psi;\ \Delta;\ \Gamma \vdash v_{1} : \mathsf{ActorState}(A) \\
    \Psi;\ \Delta;\ \Gamma \vdash v_{2} : A \\
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{setState}(v_{1}, v_{2}) : \mathsf{ActorState}(A) \lhd Q
} \qquad (\text{T-Set})
```


```math
    \frac{
      \begin{array}{c}
        \Psi;\ \Delta;\ \Gamma \vdash v_{1} : \mathsf{PID} \\
        h \in \text{dom}(\Delta_I) \\
        \Psi;\ \Delta;\ \Gamma \vdash v_{2} : \mathsf{ActorState}(B) \\
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{maty\_register}(v_{1}, \color{#6d28d9}{\mathsf{q}} \color{#4A4943}, h, v_{2}) : \mathsf{Tuple}[\mathsf{Atom}, \mathsf{ActorState}(B)] \lhd Q
    } \qquad (\text{T-Register})
```


```math
\frac{
  \begin{array}{c}
        \Psi;\ \Delta;\ \Gamma \vdash v : A_j \\
        (l = l_j) \in \{ l_i(A_i).S_i \}_{i \in I} \quad j \in I
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid \color{#6d28d9}{\mathsf{q}} \color{#4A4943} \oplus \{ l_i(A_i).S_i \}_{i \in I} \rhd \texttt{maty\_send}(\color{#6d28d9}{\mathsf{q}} \color{#4A4943},\ \{l, v\}) : \mathsf{Atom} \lhd S_j
} \qquad (\text{T-Send})
```


```math
    \frac{
      \begin{array}{c}
        h \in \text{dom}(\Delta) \\
        \Psi;\ \Delta;\ \Gamma \vdash v : \mathsf{ActorState}(B)
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{maty\_suspend}(h, v) : \bot_T \lhd \bot_S
    } \qquad (\text{T-Suspend})
```


```math
    \frac{
      \Psi;\ \Delta;\ \Gamma \vdash v : \mathsf{ActorState}(A)
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{maty\_done}(v) : \bot_{T} \lhd \bot_{S}
    } \qquad (\text{T-Done})
```




# Well-Formedness

#### Function Definitions

```math
\begin{align}
&\frac{
  \begin{array}{l}
  \forall i \in 1..n \quad \left( \vdash p_i : A_i \implies \Gamma_i \right) \\
  \forall i \neq j . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset \\
  \Gamma_{args} = \bigcup_{i=1..n} \Gamma_i \\
  \Delta;\ \Psi, \Gamma_{args} \mid \mathsf{end} \rhd e : B \lhd \mathsf{end} \\
  \end{array}
}{
  \Psi \vdash (\texttt{@spec } f(A_1, \dots, A_n) :: B \texttt{ def } f(p_1, \dots, p_n) \texttt{ do } e \texttt{ end}) \text{ ok}
}
\quad &(\text{WF-Func})
\end{align}
```

#### Handler Macros

```math
\frac{
 \begin{array}{l}
   h \mapsto S \in \Delta_M \\
   S = \text{\& } \color{#6d28d9}{\mathsf{q}} \color{#4A4943}:\{l_i(A_i).S_i\}_{i \in I} \\
   (l = l_j \land A = A_j) \in \{l_i(A_i).S_i\}_{i \in I} \quad (\text{for some } j \in I) \\ \\

   \vdash p : A \implies \Gamma' \\
   \Gamma = \Gamma', x : \mathsf{ActorState}(B) \\ \\

   \Delta;\ \Psi, \Gamma \mid S \rhd e : \bot_{T} \lhd \bot_{S} \\
 \end{array}
}{
   \Delta; \Psi \vdash ( \texttt{handler } h,\ \color{#6d28d9}{\mathsf{q}} \color{#4A4943},\ \{ l, p :: A\},\ x \texttt{ do } e \texttt{ end}) \text{ ok}
} \qquad (\text{WF-MsgHandler})
```


```math
\frac{
 \begin{array}{l}
   h \mapsto S \in \Delta_I
   \\ \\
   \forall i \in 1..n \quad \left( \vdash p_i : A_i \implies \Gamma_i \right) \\
   \forall i \neq j \in \{1..n\} . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset \\
   \Gamma_{p} = \bigcup_{i=1..n} \Gamma_i \\
   \Gamma = \Gamma_{p}, x : \mathsf{ActorState}(B) \\
   \\
   \Delta;\ \Psi, \Gamma \mid S \rhd e : \bot_{T} \lhd \bot_{S} \\

 \end{array}
}{
   \Delta; \Psi \vdash ( \texttt{init\_handler } h,\ \{ p_1 :: A_{1}, \dots, p_n :: A_{n}\},\ x \texttt{ do } e \texttt{ end}) \text{ ok}
} \qquad (\text{WF-InitHandler})
```


#### Module Definitions

```math
\frac{
  \begin{array}{l}
    \Psi = \text{make\_func\_env}(M) \\
    \Delta_M = \text{make\_msg\_handler\_env}(M) \\
    \Delta_I = \text{make\_init\_handler\_env}(M) \\
    \Delta = \Delta_M \cup \Delta_I \\
    \\
    \forall F_i \in \text{functions}(M). \quad \Psi \vdash F_i \text{ ok} \\
    \forall H_{M_j} \in \text{handlers}(M). \quad \Delta; \Psi \vdash H_{M_j} \text{ ok} \\
    \forall H_{I_k} \in \text{init\_handlers}(M). \quad \Delta; \Psi \vdash H_{I_k} \text{ ok}
  \end{array}
}{
  \vdash M \text{ ok}
} \qquad (\text{WF-Module})
```
