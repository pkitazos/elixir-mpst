---
tags:
  - v4
---
# Elixir Syntax

$$
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
&\text{Values} &v &::= b \mid [v_1 \mid v_2] \mid \{v_1, \ \dots, \ v_n\} \mid \texttt{\%}\{ (v_i \ \texttt{=>} \ v_i)_{i \in I} \} \\
&\text{Identifiers} &w &::= v \mid x \\
\\
&\text{Patterns} &p &::= w \\
&&&\mid \_ \\
&&&\mid [p_{1} \mid p_{2}] \mid [] \\
&&&\mid \{ p_{1}, \dots, p_{n} \} \mid \{\} \\
&&&\mid \%\{k_{i} \Rightarrow p_{i}, \dots \} \\
\\
&\text{Expressions} &e &::= w \\
&&&\mid w_1 \ \diamond \ w_2 \mid \texttt{not} \ w \\
&&&\mid p=e_1;\ e_2 \\
&&&\mid f(w_1, \ \dots, \ w_n) \\
&&&\mid \texttt{case} \ w \ \texttt{do} \ (p_i \rightarrow e_i)_{i \in I} \ \texttt{end} \\
&&&\mid [] \mid \ \{\} \mid \ \%\{\} \mid [w_{1} \mid w_{2}] \mid \{ w_{1}, \dots, w_{n} \} \mid \%\{k_{i} \Rightarrow w_{i}   \} \\
&&&\mid \texttt{maty\_register}(w_{1}, \color{#6d28d9}{\mathsf{q}}\ \color{#4A4943}, h, w_{2}) \\
&&&\mid \texttt{maty\_send}(\color{#6d28d9}{\mathsf{q}}\ \color{#4A4943},\ \{l, w\}) \\
&&&\mid \texttt{maty\_suspend}(h, w) \\
&&&\mid \texttt{maty\_done}(w) \\
\\
&\text{Binary Operators} &\diamond &::=  \texttt{<} \mid \texttt{>} \mid \texttt{<=}  \mid \texttt{>=} \mid \texttt{==} \mid \texttt{!=} \mid \texttt{+} \mid \texttt{-} \mid \texttt{*} \mid \texttt{/} \mid \texttt{<>} \mid \texttt{and} \mid  \texttt{or}
\end{align}
$$


# Types & Session Types

## Types

$$
\begin{align}
&\text{Optional Types} &T &::= A \mid \bot_{T} \\
&\text{Base Types} &C &::= \mathsf{Atom} \mid \mathsf{Nil} \mid \mathsf{Bool} \mid \mathsf{Number} \mid \mathsf{Binary} \mid \mathsf{Date} \mid \mathsf{PID} \mid \mathsf{Ref} \\
&\text{Types} &A, B &::= C \\ 
&&&\mid \mathsf{List}[A] \mid \mathsf{Tuple}[\widetilde{A}] \mid \mathsf{Map}[C, A] \\
&&&\mid (\widetilde{A}) \rightarrow B \\ 
&&&\mid \mathsf{Handler}(S^?) \\ 
&&&\mid \mathsf{InitHandler}(S) \\ 
\end{align}
$$

## Session Types

$$
\begin{align}
Q &::= S \mid \bot_{S} \\
\\
S &::= \mathsf{end} \mid S^! \mid S^? \mid h \\
\\
S^! &::= \oplus p:\{l_i(A_i).S_i\}_{i \in I} &&(\text{Internal Choice: Send } l_j(A_j) \text{ to } p\text{, continue as } S_j) \\
S^? &::= \& p:\{l_i(A_i).S_i\}_{i \in I} &&(\text{External Choice: Expect } l_j(A_j) \text{ from } p\text{, continue as } S_j) \\
\end{align}
$$


## Join Operator (⊔) for Branch Outcomes

>[!note] note for me
> explicitly mention that the $\bot$ operator is commutative and associative


**For Session Types** ($\bot = \bot_{S}$)
$$
\begin{align}
	Q \sqcup Q &= Q \\
	\bot \sqcup Q &= Q \\
	Q \sqcup \bot &= Q \\
	\bot \sqcup \bot &= \bot \\
\end{align}
$$

**For Types** ($\bot = \bot_{T}$)
$$
\begin{align}
	T \sqcup T &= T \\
	\bot \sqcup T &= T \\
	T \sqcup \bot &= T \\
	\bot \sqcup \bot &= \bot \\
\end{align}
$$

**For Session Type - Type Pairs**
$$
(Q_a, \ T_a) \ \sqcup \ (Q_b, \ T_b) = (Q_a \sqcup Q_b, \ T_a \sqcup T_b)
$$

---

# Meta-level Module Functions

$$
\begin{align}
&\text{name}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &m \\

&\text{session\_types}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &\widetilde{K} \\

&\text{init\_handlers}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &\widetilde{H}_{\text{I}} \\

&\text{handlers}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &\widetilde{H}_{\text{M}} \\

&\text{functions}(\texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) &= \quad &\widetilde{F}
\end{align}
$$


$$
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
$$

# Environments

#### Variable Binding Environment

$$
\Large \Gamma = \cdot \mid \Gamma, x:A 
$$

#### Module Function Type Environment

$$
\Large \Psi = \{ (f, n) \mapsto (A_1, \dots, A_n) \to B \}
$$

#### Module Handler Environment

$$
\Large
\Delta = \{ h \mapsto S \} \\
$$




# Pattern Matching

> [!note] note for me
> framebox around the typing judgements

$$
\Large \vdash p : A \implies \Gamma'
$$

$$
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
$$



# Value and Identifier Typing

$$
\Large \Psi;\ \Delta;\ \Gamma \vdash w : A
$$
> [!question]
> I use $w$ in the typing judgement but I'm actually also typechecking handler names $h$  and functions $f$ not just $w$ using this judgement. Should I create like a $w' = w \mid h \mid f$ and use $w'$ instead or is this okay as is?


$$
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
$$


$$
\begin{align}
&\frac{
	x:A \in \Gamma
}{
	\Psi;\ \Delta;\ \Gamma \vdash x:A
}\quad &(\text{TV-Var})
\\ \\
&\frac{
    \Psi(f, n) = (A_1, \dots, A_n) \to B
}{ \\
    \Psi;\ \Delta;\ \Gamma \vdash f : (A_1, \dots, A_n) \to B
} \quad &(\text{TV-Func})
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
$$



# Computation Typing

$$
\Large \Psi; \ \Delta; \ \Gamma \mid Q_1 \rhd e : T \lhd Q_2
$$
---
### Pure Computations

**(T-Not)**
$$
    \frac{
      \Psi;\ \Delta;\ \Gamma \vdash w : \mathsf{Bool}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd (\texttt{not } w) : \mathsf{Bool} \lhd Q
    }
$$

**(T-EmptyList)**
$$
\frac{
}{
   \Psi;\ \Delta;\ \Gamma \mid Q \rhd [] : \mathsf{List}[A] \lhd Q % Type 'A' needs context/inference
}
$$

**(T-Cons)**
$$
\frac{
  \begin{array}{c}
    \Psi;\ \Delta;\ \Gamma \vdash w_{1} : A \\
     \Psi;\ \Delta;\ \Gamma \vdash w_{2}: \mathsf{List}[A]
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd [w_1 | w_2] : \mathsf{List}[A] \lhd Q
}
$$

**(T-TupleEmpty)**
$$
\frac{
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \{\} : \mathsf{Tuple}[] \lhd Q
}
$$

**(T-Tuple)**
$$
\frac{
  (\Psi;\ \Delta;\ \Gamma \vdash w_i : A_i)_{i\in1..n}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \{w_1, \dots, w_n\} : \mathsf{Tuple}[A_1, \dots, A_n] \lhd Q
}
$$

**(T-EmptyMap)**
$$
\frac{
}{
   \Psi;\ \Delta;\ \Gamma \mid Q \rhd \%\{\} : \mathsf{Map}[C, A] \lhd Q % Types 'C', 'A' needs context/inference
}
$$

**(T-Map)**
$$
\frac{
  \begin{array}{c}
    (\Psi;\ \Delta;\ \Gamma \vdash k_i : C)_{i \in I} \\
    (\Psi;\ \Delta;\ \Gamma \vdash w_i : A)_{i \in I}
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \%\{ (k_i \Rightarrow w_i)_{i \in I} \} : \mathsf{Map}[C, A] \lhd Q
}
$$

**(T-Op)**
$$
    \frac{
      \begin{array}{c}
        \Psi;\ \Delta;\ \Gamma \vdash w_1 : A \\
        \Psi;\ \Delta;\ \Gamma \vdash w_2 : B \\
        \text{OpTypeRel}(\diamond, A, B, C)
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd w_1 \diamond w_2 : C \lhd Q
    }
$$

$$
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
$$

**(T-App)**
$$
    \frac{
      \begin{array}{c}
        (\Psi;\ \Delta;\ \Gamma \vdash w_i : A_i)_{i\in1..n} \\
        \Psi(f, n) = (A_{1},\dots,A_{n}) \to B
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd f(w_{1}, \dots,w_{n}) : B \lhd Q
    } \quad (\text{T-App})
$$

---


### Potentially Effectful Computations

**(T-Let)**
$$
    \frac{
    \begin{array}{c}
       \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd e_1 : A \lhd Q_2 \\
       \vdash p : A \implies \Gamma' \\
       \Psi;\ \Delta;\ \Gamma, \Gamma' \mid Q_2 \rhd e_2 : T \lhd Q_3
    \end{array}
    }{
       \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd p = e_1; e_2 : T \lhd Q_3
    }
$$

**(T-Case)**
$$
    \frac{
      \begin{array}{c}
        \Psi;\ \Delta;\ \Gamma \vdash w : A \\
        \forall i \in I.\ (
          \vdash p_i : A \implies \Gamma_i
          \quad \land \quad
          \Psi;\ \Delta;\ \Gamma, \Gamma_i \mid Q_1 \rhd e_i : T_i \lhd Q'_{i}
        ) \\
        (T, Q_2) = \bigsqcup_{i \in I} (T_i, Q'_{i})
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd \texttt{case } w \texttt{ do } (p_i \to e_i)_{i \in I} \text{ end} : T \lhd Q_2
    }
$$

**(T-Get)**
$$
\frac{
  \Psi;\ \Delta;\ \Gamma \vdash w : \mathsf{ActorState}(A)
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{getState}(w) : A \lhd Q
}
$$

**(T-Set)**
$$
\frac{
  \begin{array}{c}
    \Psi;\ \Delta;\ \Gamma \vdash w_{1} : \mathsf{ActorState}(A) \\
    \Psi;\ \Delta;\ \Gamma \vdash w_{2} : A \\
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{setState}(w_{1}, w_{2}) : \mathsf{ActorState}(A) \lhd Q
}
$$

>[!question]
>I totally forgot, but should the `getState` and `setState` functions be defined in the syntax? Or are they special functions just to explain that interacting with the actor state doesn't affect the session?

**(T-Register)**
$$
    \frac{
      \begin{array}{c}
        \Psi;\ \Delta;\ \Gamma \vdash w_{1} : \mathsf{PID} \\
        h \in \text{dom}(\Delta_I) \\
        \Psi;\ \Delta;\ \Gamma \vdash w_{2} : \mathsf{ActorState}(B) \\
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{maty\_register}(w_{1}, \color{#6d28d9}{\mathsf{q}} \color{#4A4943}, h, w_{2}) : \mathsf{Tuple}[\mathsf{Atom}, \mathsf{ActorState}(B)] \lhd Q
    }
$$

**(T-Send)**
$$
\frac{
  \begin{array}{c}
        \Psi;\ \Delta;\ \Gamma \vdash w : A_j \\
        (l = l_j) \in \{ l_i(A_i).S_i \}_{i \in I} \quad j \in I
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid \color{#6d28d9}{\mathsf{q}} \color{#4A4943} \oplus \{ l_i(A_i).S_i \}_{i \in I} \rhd \texttt{maty\_send}(\color{#6d28d9}{\mathsf{q}} \color{#4A4943},\ \{l, w\}) : \mathsf{Atom} \lhd S_j
}
$$

**(T-Suspend)**
$$
    \frac{
      \begin{array}{c}
        h \in \text{dom}(\Delta) \\
        \Psi;\ \Delta;\ \Gamma \vdash w : \mathsf{ActorState}(B)
      \end{array}
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{maty\_suspend}(h, w) : \bot_T \lhd \bot_S
    }
$$

**(T-Done)**
$$
    \frac{
      \Psi;\ \Delta;\ \Gamma \vdash w : \mathsf{ActorState}(A)
    }{
      \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{maty\_done}(w) : \mathsf{ActorState}(A) \lhd \mathsf{end}
    }
$$




# Well-Formedness

#### Function Definitions

$$
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
$$

#### Handler Macros

**(WF-MessageHandler)**
$$
\frac{
 \begin{array}{l}
   h \mapsto S \in \Delta_M \\
   S = \text{\& } q:\{l_i(A_i).S_i\}_{i \in I} \\
   (l = l_j \land A = A_j) \in \{l_i(A_i).S_i\}_{i \in I} \quad (\text{for some } j \in I) \\ \\

   \vdash p : A \implies \Gamma' \\
   \Gamma = \Gamma', x : \mathsf{ActorState}(B) \\ \\

   \Delta;\ \Psi, \Gamma \mid S \rhd e : T' \lhd Q' \\
   (T' = \mathsf{ActorState}(B') \land Q' = \mathsf{end}) \lor (T' = \bot_T \land Q' = \bot_S) \\
 \end{array}
}{
   \Delta; \Psi \vdash ( \texttt{handler } h,\ \color{#6d28d9}{\mathsf{q}} \color{#4A4943},\ \{ l, p :: A\},\ x \texttt{ do } e \texttt{ end}) \text{ ok}
}
$$

**(WF-InitHandler)**
$$
\frac{
 \begin{array}{l}
   h \mapsto S \in \Delta_I \\
   S \neq S^{\text{end}} \land S \neq S'^?
   \\ \\
   \forall i \in 1..n \quad \left( \vdash p_i : A_i \implies \Gamma_i \right) \\
   \forall i \neq j \in \{1..n\} . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset \\
   \Gamma_{p} = \bigcup_{i=1..n} \Gamma_i \\
   \Gamma = \Gamma_{p}, x : \mathsf{ActorState}(B) \\
   \\
   \Delta;\ \Psi, \Gamma \mid S \rhd e : T' \lhd Q' \\
   T' = \bot_T \land Q' = \bot_S \\
 \end{array}
}{
   \Delta; \Psi \vdash ( \texttt{init\_handler } h,\ \{ p_1 :: A_{1}, \dots, p_n :: A_{n}\},\ x \texttt{ do } e \texttt{ end}) \text{ ok}
}
$$


#### Module Definitions

$$
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
}
\quad (\text{WF-Module})
$$
