---
tags:
  - v3
---
# Elixir Syntax

$$
\begin{align}
&\text{Variable} &x \\
&\text{Function} &f_n \\
&\text{Role} &\text{q},\ \text{r} \\
&\text{Label} &l \\
&\text{Handler} &h \\
\\
&\text{Module} &M &::= \texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}\\
&\text{Session Annotation} &K &::= \texttt{@st} \ \{h, S\} \\
&\text{Init Handler} &H_{\text{I}} &::= \texttt{init\_handler } h,\ \{ p_1 :: A_{1},\ \dots,\ p_n :: A_{n}\},\ p_{\sigma} \texttt{ do } e \texttt{ end} \\
&\text{Message Handler} &H_{\text{M}} &::= \texttt{handler } h,\ \text{q},\ \{ l, p_{\text{msg}} :: A_{\text{msg}}\},\ p_{\sigma} \texttt{ do } e \texttt{ end} \\
&\text{Function} &F &::= \texttt{@spec} \ f(\widetilde{A}) :: B \ \texttt{def} \ f(p_1,\ \dots,\ p_n) \  \texttt{do} \ e \ \texttt{end}\\
\\
&\text{Basic Values} &b &::= \text{atom} \mid \text{nil} \mid \text{boolean} \mid \text{number} \mid \text{binary} \mid \text{date} \mid \text{PID} \mid \text{Ref} \mid [] \\
&\text{Values} &v &::= b \mid [v_1 \mid v_2] \mid \{v_1, \ \dots, \ v_n\} \mid \texttt{\%}\{ (v_i \ \texttt{=>} \ v_i)_{i \in I} \} \\
\\
&\text{Patterns} &p &::= v \\
&&&\mid x \mid \_ \\
&&&\mid [p_{1} \mid p_{2}] \mid [] \\
&&&\mid \{ p_{1}, \dots, p_{n} \} \mid \{\} \\
&&&\mid \%\{k_{i} \Rightarrow p_{i}, \dots \} \\
\\
&\text{Expressions} &e &::= v \\
&&&\mid x \\
&&&\mid e_1 \ \diamond \ e_2 \mid \texttt{not} \ e \\
&&&\mid p=e_1;\ e_2 \\
&&&\mid f(e_1, \ \dots, \ e_n) \\
&&&\mid \texttt{case} \ e \ \texttt{do} \ (p_i \rightarrow e_i)_{i \in I} \ \texttt{end} \\
&&&\mid [] \mid \ \{\} \mid \ \%\{\} \mid [e_{1} \mid e_{2}] \mid \{ e_{1}, \dots, e_{n} \} \mid \%\{k_{i} \Rightarrow e_{i}   \} \\
&&&\mid \texttt{maty\_register}(e_{\text{AP}}, \text{q}, h, e_{\sigma}) \\
&&&\mid \texttt{maty\_send}(\text{q},\ \{l, e\}) \\
&&&\mid \texttt{maty\_suspend}(h, e_{\sigma}) \\
&&&\mid \texttt{maty\_done}(e_{\sigma}) \\
\\
&\text{Binary Operators} &\diamond &::=  \texttt{<} \mid \texttt{>} \mid \texttt{<=}  \mid \texttt{>=} \mid \texttt{==} \mid \texttt{!=} \mid \texttt{+} \mid \texttt{-} \mid \texttt{*} \mid \texttt{/} \mid \texttt{<>} \mid \texttt{and} \mid  \texttt{or}
\end{align}
$$

## Types

$$
\begin{align}
&\text{Optional Types} &T &::= A \mid \bot_{T} \\
&\text{Base Types} &C &::= \text{Atom} \mid \text{Nil} \mid \text{Bool} \mid \text{Number} \mid \text{Binary} \mid \text{Date} \mid \text{PID} \mid \text{Ref} \\
&\text{Types} &A, B &::= C \\ 
&&&\mid \text{List}[A] \mid \text{Tuple}[\widetilde{A}] \mid \text{Map}[C, A] \\
&&&\mid (\widetilde{A}) \rightarrow B \\ 
&&&\mid \text{Handler}(S^?) \\ 
&&&\mid \text{InitHandler}(S) \\ 
\end{align}
$$

## Session Types

$$
\begin{align}
Q &::= S \mid \bot_{S} \\
\\
S &::= S^{\text{end}} \mid S^! \mid S^? \mid h \\

S^{\text{end}} &::= \text{end} &&(\text{Protocol finished successfully}) \\
S^! &::= \oplus p:\{l_i(A_i).S_i\}_{i \in I} &&(\text{Internal Choice: Send } l_j(A_j) \text{ to } p\text{, continue as } S_j) \\
S^? &::= \& p:\{l_i(A_i).S_i\}_{i \in I} &&(\text{External Choice: Expect } l_j(A_j) \text{ from } p\text{, continue as } S_j) \\
S^{h} &::= h &&(\text{Continue protocol at handler } h\text{'s session type}) \\
\end{align}
$$

>[!question]
>- What do I need to include along with $h$ here to make it clear that it's a label which is used to lookup a session type in $\Delta$
>- I don't know that my choice of $T$ and $Q$ are the best here, what would you recommend I change these to? I considered using $\text{Maybe}(A)$ and $\text{Maybe}(S)$ but thought that might be too verbose?


## Join Operator (`⊔`) for Branch Outcomes
**For Session Types**
$$
\begin{align}
	Q \sqcup Q &= Q \\
	\bot \sqcup Q &= Q \\
	Q \sqcup \bot &= Q \\
	\bot \sqcup \bot &= \bot \\
\end{align}
$$

**For Types**
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


# Environments

### Variable Binding Environment

$$
\begin{align} \\
\Gamma &= \cdot \mid \Gamma, x:A \\
\end{align}
$$

### Module Function Type Environment

$$
\Psi = \{ (f, n) \mapsto (A_1, \dots, A_n) \to B \mid
          \texttt{@spec} \ f(A_1, \dots,A_n) :: B \ \texttt{def} \ f(p_1, \dots, p_n) \ \texttt{do} \ e \ \texttt{end} \in \widetilde{F}
       \}
$$

### Module Handler Environment

$$
\begin{align}
&\Delta_M &&= \quad \{ h \mapsto S \mid \texttt{handler } h \ \dots \text{ end} \in \widetilde{H}_{\text{M}},\ \texttt{@st } \{ h_{K},\ S \} \in \widetilde{K},\ h=h_{K} \} \\
&\Delta_I &&= \quad \{ h \mapsto S \mid \texttt{init\_handler } h \ \dots \text{ end} \in \widetilde{H}_{\text{I}},\ \texttt{@st } \{ h_{K},\ S \} \in \widetilde{K},\ h=h_{K} \} \\
&\Delta &&= \quad \Delta_H \cup \Delta_I
\end{align}
$$




## Pattern Matching

$$
\Large \vdash p : A \implies \Gamma'
$$

$$
\begin{align*}
&\frac{
  % No premises
}{
  \vdash x : A \implies \{x:A\}
} && (\text{Pat-Var}) \\ \\
&\frac{
  % No premises
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
    \vdash p_2 : \text{List}[A] \implies \Gamma_2 \\
    \text{dom}(\Gamma_1) \cap \text{dom}(\Gamma_2) = \emptyset
  \end{array}
}{
  \vdash [p_1 | p_2] : \text{List}[A] \implies \Gamma_1 \cup \Gamma_2
} && (\text{Pat-Cons}) \\ \\
&\frac{
  B = \text{List}[A'] \quad \text{(for some A')}
}{
  \vdash [] : B \implies \cdot
} && (\text{Pat-EmptyList}) \\ \\
&\frac{
  \begin{array}{c}
    B = \text{Tuple}[A_1, \dots, A_n] \\
    (\vdash p_i : A_i \implies \Gamma_i)_{i=1..n} \\
    \forall i \neq j \in \{1..n\} . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset
  \end{array}
}{
   \vdash \{p_1, \dots, p_n\} : B \implies \bigcup_{i=1..n} \Gamma_i
} && (\text{Pat-Tuple}) \\ \\
&\frac{
   A = \text{Tuple}[]
}{
   \vdash \{\} : A \implies \cdot
} && (\text{Pat-EmptyTuple}) \\ \\
&\frac{
  \begin{array}{c}
    B = \text{Map}[C, A] \\
    (\Gamma \vdash k_i : C)_{i \in I} \\
    (\vdash p_i : A \implies \Gamma_i)_{i \in I} \\
    \forall i \neq j \in I . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset
  \end{array}
}{
  \vdash \%\{ (k_i \Rightarrow p_i) \}_{i \in I} : B \implies \bigcup_{i \in I} \Gamma_i
} && (\text{Pat-Map}) \\ \\
&\frac{
   A = \text{Map}[C, A'] \quad \text{(for some C, A')}
}{
   \vdash \%\{\} : A \implies \cdot
} && (\text{Pat-EmptyMap})
\end{align*}
$$


## Value Typing

$$
\Large \Gamma \vdash v : A
$$

$$
\Large \Psi;\ \Delta;\ \Gamma \vdash v : A
$$
> [!question]
> With some of the changes I've made to the syntax is either typing judgement still correct?

$$
\begin{align*}
&\frac{
  b \in \{\text{atom}, \text{nil}, \text{bool}, \text{number}, \text{binary}, \text{date}, \text{PID}, \text{Ref}\}
  \quad
  \text{TypeOfLiteral}(b) = C
}{
  \Gamma \vdash b : C
} && (\text{Val-BaseLit}) \\ \\
&\frac{
  % No premises
}{
  \Gamma \vdash [] : \text{List}[\text{Nil}] % Default type
} && (\text{Val-EmptyList}) \\ \\
&\frac{
  \Gamma \vdash v_1 : A \quad \Gamma \vdash v_2 : \text{List}[A]
}{
  \Gamma \vdash [v_1 | v_2] : \text{List}[A]
} && (\text{Val-Cons}) \\ \\
&\frac{
  (\Gamma \vdash v_i : A_i)_{i=1..n}
}{
  \Gamma \vdash \{v_1, \dots, v_n\} : \text{Tuple}[A_1, \dots, A_n]
} && (\text{Val-Tuple}) \\ \\
&\frac{
  % No premises
}{
  \Gamma \vdash \{\} : \text{Tuple}[]
} && (\text{Val-EmptyTuple}) \\ \\
&\frac{
 (\Gamma \vdash k_j : C)_{j \in J} \quad (\Gamma \vdash v_j : A)_{j \in J} \quad (C \in \text{BaseTypes})
}{
 \Gamma \vdash \%\{ (k_j \Rightarrow v_j)_{j \in J} \} : \text{Map}[C, A]
} && (\text{Val-Map}) \\ \\
&\frac{
  % No premises
}{
  \Gamma \vdash \%\{\} : \text{Map}[\text{Atom}, \text{Nil}] % Example default type
} && (\text{Val-EmptyMap})
\end{align*}
$$

> [!question]
> I'm not sure how to write the typing rule for Base Literals or even if I'm defining those properly in my Elixir syntax


$$
\begin{align}
&\frac{
	x:A \in \Gamma
}{
	\Psi;\ \Delta;\ \Gamma \vdash x:A
}\quad &(\text{TV-Var})
\\ \\
&\frac{
	c \text{ has base type } C
}{
\Psi;\ \Delta;\ \Gamma \vdash c:C
}\quad &(\text{TV-Literal})
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
    \Psi;\ \Delta;\ \Gamma \vdash h : \text{Handler}(S^{?})
} \quad &(\text{TV-MsgHandler})
\\ \\
&\frac{  \\
    h \mapsto S \in \Delta_I \\
}{ \\
    \Psi;\ \Delta;\ \Gamma \vdash h : \text{InitHandler}(S) \\
} \quad &(\text{TV-InitHandler})
\end{align}
$$



## Computation Typing

$$
\Large \Psi; \ \Delta; \ \Gamma \mid Q_1 \rhd e : T \lhd Q_2
$$

### Pure Computations

**(T-Not)**
$$
\frac{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd e : \text{Bool} \lhd Q
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd (\texttt{not } e) : \text{Bool} \lhd Q
}
$$

**(T-EmptyList)**
$$
\frac{
  % No premises
}{
   \Psi;\ \Delta;\ \Gamma \mid Q \rhd [] : \text{List}[A] \lhd Q % Type 'A' needs context/inference
}
$$

**(T-Cons)**
$$
\frac{
  \begin{array}{c}
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_1 : A \lhd Q \\
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_2 : \text{List}[A] \lhd Q
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd [e_1 | e_2] : \text{List}[A] \lhd Q
}
$$

**(T-TupleEmpty)**
$$
\frac{
  % No premises
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \{\} : \text{Tuple}[] \lhd Q
}
$$

**(T-Tuple)**
$$
\frac{
  (\Psi;\ \Delta;\ \Gamma \mid Q \rhd e_i : A_i \lhd Q)_{i=1..n}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \{e_1, \dots, e_n\} : \text{Tuple}[A_1, \dots, A_n] \lhd Q
}
$$

**(T-EmptyMap)**
$$
\frac{
  % No premises
}{
   \Psi;\ \Delta;\ \Gamma \mid Q \rhd \%\{\} : \text{Map}[C, A] \lhd Q % Types 'C', 'A' needs context/inference
}
$$

**(T-Map)**
$$
\frac{
  \begin{array}{c}
    (\Psi;\ \Delta;\ \Gamma \mid Q \rhd k_i : C \lhd Q)_{i \in I} \\
    (\Psi;\ \Delta;\ \Gamma \mid Q \rhd e_i : A \lhd Q)_{i \in I}
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \%\{ (k_i \Rightarrow e_i)_{i \in I} \} : \text{Map}[C, A] \lhd Q
}
$$


> [!question]
> I seem to remember we said something about not having to explicitly write out every rule, but I don't remember how to represent that

$$
\frac{
  \begin{array}{c}
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_1 : A \lhd Q \\
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_2 : B \lhd Q \\
    \text{OpTypeRel}(\diamond, A, B, C) % Check relation holds for types A, B; determines C
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_1 \diamond e_2 : C \lhd Q
} \quad (\text{T-OpRel})
$$

$$
\begin{gathered}
\text{OpTypeRel}(\diamond, A, B, C) \\
\iff
\\
\begin{array}{l}
 (\diamond \in \{+, -, *, /\} \land A = \text{Number} \land B = \text{Number} \land C = \text{Number}) \quad \lor \\
 (\diamond = \texttt{<>} \land A = \text{Binary} \land B = \text{Binary} \land C = \text{Binary}) \quad \lor \\
 (\diamond \in \{\texttt{and}, \texttt{or}\} \land A = \text{Bool} \land B = \text{Bool} \land C = \text{Bool}) \quad \lor \\
 (\diamond \in \{\lt, \gt, \leq, \geq \} \land A = \text{Number} \land B = \text{Number} \land C = \text{Bool}) \quad \lor \\
 (\diamond \in \{==, \neq \} \land A = B \land C = \text{Bool})
\end{array}
\end{gathered}
$$

> [!question]
> Not sure how to properly enforce the typing constraints I want for my binary operators without having to write out 5 different rules. Could I do this?



### Potentially Effectful Computations

**(T-MatchSeqPure)**
$$
\frac{
  \begin{array}{c}
   \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd e_1 : A \lhd Q_1 \\
   \vdash p : A \implies \Gamma' \\
   \Psi;\ \Delta;\ \Gamma, \Gamma' \mid Q_1 \rhd e_2 : T \lhd Q_2
  \end{array}
}{
   \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd p = e_1; e_2 : T \lhd Q_2
}
$$

**(T-App)**
$$
\frac{
  \begin{array}{c}
    (\Psi;\ \Delta;\ \Gamma \mid Q \rhd e_i : A_i \lhd Q)_{i=1..n} \\
    \Psi(f, n) = (A_{1},\dots,A_{n}) \to B
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd f(e_{1}, \dots,e_{n}) : B \lhd Q
}
$$

**(T-Case)**
$$
\frac{
  \begin{array}{c}
    \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd e : A \lhd Q_1 \\ \\
    \forall i \in I.\ ( \vdash p_i : A \implies \Gamma_i \land \Psi;\ \Delta;\ \Gamma, \Gamma_i \mid Q_1 \rhd e_i : T_i \lhd Q'_{i} ) \\ \\
    (T, Q_2) = \bigsqcup_{i \in I} (T_i, Q'_{i})
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q_1 \rhd \texttt{case } e \texttt{ do } (p_i \to e_i)_{i \in I} \text{ end} : T \lhd Q_2
}
$$

**(T-Get)**
$$
\frac{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_{\sigma} : \text{ActorState}(A) \lhd Q
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{getState}(e_{\sigma}) : A \lhd Q
}
$$

**(T-Set)**
$$
\frac{
  \begin{array}{c}
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd e : A \lhd Q \\
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_{\sigma} : \text{ActorState}(A) \lhd Q
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{setState}(e, e_{\sigma}) : \text{ActorState}(A) \lhd Q
}
$$

**(T-Register)**
$$
\frac{
  \begin{array}{c}
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_{\text{AP}} : \text{PID} \lhd Q \\
    h \in \text{dom}(\Delta_I) \\
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_{\sigma} : \text{ActorState}(B) \lhd Q
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{maty\_register}(e_{\text{AP}}, \text{q}, h, e_{\sigma}) : \text{Tuple}[\text{Atom}, \text{ActorState}(B)] \lhd Q
}
$$

**(T-Send)**
$$
\frac{
  \begin{array}{c}
    \Psi;\ \Delta;\ \Gamma \mid q \oplus \{ l_i(A_i).S_i \}_{i \in I} \rhd e : A_j \lhd q \oplus \{ l_i(A_i).S_i \}_{i \in I} \\
    (l = l_j) \in \{ l_i(A_i).S_i \}_{i \in I} \quad j \in I
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid q \oplus \{ l_i(A_i).S_i \}_{i \in I} \rhd \texttt{maty\_send}(\text{q},\ \{l, e\}) : \text{Atom} \lhd S_j
}
$$

**(T-Suspend)**
$$
\frac{
  \begin{array}{c}
    h \in \text{dom}(\Delta_{\text{M}}) \\
    \Psi;\ \Delta;\ \Gamma \mid Q \rhd e_{\sigma} : \text{ActorState}(B) \lhd Q
  \end{array}
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{maty\_suspend}(h, e_{\sigma}) : \bot_T \lhd \bot_S
}
$$

**(T-Done)**
$$
\frac{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd e : \text{ActorState}(A) \lhd Q
}{
  \Psi;\ \Delta;\ \Gamma \mid Q \rhd \texttt{maty\_done}(e) : \text{ActorState}(A) \lhd S^{\text{end}}
}
$$




## Well-Formedness

### Function Definitions

$$
\begin{align}
&\frac{
  \begin{array}{l}
  \forall i \in 1..n \quad \left( \vdash p_i : A_i \implies \Gamma_i \right) \\
  \forall i \neq j . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset \\
  \Gamma_{args} = \bigcup_{i=1..n} \Gamma_i \\
  \Psi, \Gamma_{args} \vdash e : B' \\
  B' = B
  \end{array}
}{
  \Psi \vdash (\texttt{@spec } f(A_1, \dots, A_n) :: B \texttt{ def } f(p_1, \dots, p_n) \texttt{ do } e \texttt{ end}) \text{ ok}
}
\quad &(\text{WF-Func})
\end{align}
$$

### Handler Macros

$$
\frac{
 \begin{array}{l}
   h \mapsto S \in \Delta_M \\
   S = \text{\& } q':\{l_i(A_i).S_i\}_{i \in I} \\
   q = q' \\
   (l = l_j \land A_{msg} = A_j) \in \{l_i(A_i).S_i\}_{i \in I} \quad (\text{for some } j \in I) \\
   \\
   \vdash p_{\text{msg}} : A_{\text{msg}} \implies \Gamma_{msg} \\
   \vdash p_{\sigma} : \text{ActorState}(B) \implies \Gamma_{\sigma} \\
   \\
   \text{dom}(\Gamma_{\text{msg}}) \cap \text{dom}(\Gamma_{\sigma}) = \emptyset \\
   \Gamma_{args} = \Gamma_{\text{msg}} \cup \Gamma_{\sigma} \\
   \\
   \Delta;\ \Psi, \Gamma_{args} \mid S \rhd e : T' \lhd Q' \\
   \\
   (T' = \text{ActorState}(C) \land Q' = S^{\text{end}}) \lor (T' = \bot_T \land Q' = \bot_S) \\
 \end{array}
}{
   \Delta; \Psi \vdash ( \texttt{handler } h,\ \text{q},\ \{ l, p_{\text{msg}} :: A_{\text{msg}}\},\ p_{\sigma} \texttt{ do } e \texttt{ end}) \text{ ok}
}
\quad (\text{WF-Handler})
$$

$$
\frac{
 \begin{array}{l}
   h \mapsto S \in \Delta_I \\
   S \neq S^{\text{end}} \land S \neq S'^? \\
   \\
   \forall i \in 1..n \quad \left( \vdash p_i : A_i \implies \Gamma_i \right) \\
   \forall i \neq j . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset \\
   \Gamma_{p} = \bigcup_{i=1..n} \Gamma_i \\
   \\
   \vdash p_{\sigma} : \text{ActorState}(B) \implies \Gamma_{\sigma} \\
   \text{dom}(\Gamma_{p}) \cap \text{dom}(\Gamma_{\sigma}) = \emptyset \\
   \Gamma_{args} = \Gamma_{p} \cup \Gamma_{\sigma} \\
   \\
   \Delta;\ \Psi, \Gamma_{args} \mid S \rhd e : T' \lhd Q' \\
   \\
   T' = \bot_T \land Q' = \bot_S \\
 \end{array}
}{
   \Delta; \Psi \vdash ( \texttt{init\_handler } h,\ \{ p_1 :: A_{1}, \dots, p_n :: A_{n}\},\ p_{\sigma} \texttt{ do } e \texttt{ end}) \text{ ok}
}
\quad (\text{WF-InitHandler})
$$


## Module Definitions
$$
\frac{
  \begin{array}{l}
    \\
    \Psi = \{ (f, n) \mapsto (A_1..A_n) \to B \mid \dots \in \widetilde{F} \} \\
    \Delta_M = \{ h \mapsto S \mid \dots \in \widetilde{H}_{\text{M}}, \dots \in \widetilde{K} \} \\
    \Delta_I = \{ h \mapsto S \mid \dots \in \widetilde{H}_{\text{I}}, \dots \in \widetilde{K} \} \\
    \Delta = \Delta_M \cup \Delta_I \\
    \\
    \forall F_i \in \widetilde{F}. \quad \Psi \vdash F_i \text{ ok} \\
    \\
    \forall H_{M_j} \in \widetilde{H}_{\text{M}}. \quad \Delta; \Psi \vdash H_{M_j} \text{ ok} \\
    \\
    \forall H_{I_k} \in \widetilde{H}_{\text{I}}. \quad \Delta; \Psi \vdash H_{I_k} \text{ ok} \\

  \end{array}
}{
    \vdash (\texttt{defmodule} \ m \ \texttt{do} \ \widetilde{K} \ \widetilde{H}_{\text{I}} \ \widetilde{H}_{\text{M}} \ \widetilde{F} \ \texttt{end}) \text{ ok}
}
\quad (\text{WF-Module})
$$