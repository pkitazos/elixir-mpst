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
&\text{Module} &M &::= \texttt{defmodule} \ m \  \texttt{do} \  \widetilde{K} \ \widetilde{I} \ \widetilde{H} \ \widetilde{F} \ \texttt{end}\\
&\text{Session Annotation} &K &::= \texttt{@st} \ \{h, S\} \\
&\text{InitHandler} &I &::= \texttt{init\_handler } h,\ \{ p_1 :: A_{1},\ \dots,\ p_n :: A_{n}\},\ p_{\sigma} \texttt{ do } e \texttt{ end} \\
&\text{Handler} &H &::= \texttt{handler } h,\ \text{q},\ \{ l, p_{\text{msg}} :: A_{\text{msg}}\},\ p_{\sigma} \texttt{ do } e \texttt{ end} \\
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
&&&\mid [] \mid \ \%\{\} \mid [e_{1} \mid e_{2}] \mid \{ e_{1}, \dots, e_{n} \} \mid \%\{k_{i} \Rightarrow e_{i}   \} \\
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
&\text{Base Types} &C &::= \text{Atom} \mid \text{Nil} \mid \text{Bool} \mid \text{Number} \mid \text{Binary} \mid \text{Date} \mid \text{PID} \mid \text{Ref} \mid \text{Date} \\
&\text{Types} &A, B &::= C \\ 
&&&\mid \text{List}[A] \mid \text{Tuple}[A_1, \dots, A_n] \mid \text{Map}[C, A] \\
&&&\mid f(A_1,\dots,A_n) \rightarrow B \\ 
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
	S \sqcup S &= S \\
	\bot \sqcup S &= S \\
	S \sqcup \bot &= S \\
	\bot \sqcup \bot &= \bot \\
\end{align}
$$

**For Types**
$$
\begin{align}
	A \sqcup A &= A \\
	\bot \sqcup A &= A \\
	A \sqcup \bot &= A \\
	\bot \sqcup \bot &= \bot \\
\end{align}
$$

**For Session Type - Type Pairs**
$$
(Q_a, \ T_a) \ \bigsqcup \ (Q_b, \ T_b) = (Q_a \sqcup Q_b, \ T_a \sqcup T_b)
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
\begin{align}
\Psi &= \{ \ f_{n} \mapsto f(A_{i}, \dots, A_{n}) \to B \mid \texttt{@spec} \ f(A_{i}, \dots,A_{n}) :: B \ \texttt{def} \ f \  \texttt{do} \ t \ \texttt{end} \in \widetilde{F} \ \}
\end{align}
$$

### Module Handler Environment

$$
\begin{align}
&\Delta_H &&= \quad \{ h \mapsto S \mid \texttt{handler } h \ \dots \text{ end} \in \widetilde{H},\ \texttt{@st } \{ h_{K},\ S \} \in \widetilde{K},\ h=h_{K} \} \\
&\Delta_I &&= \quad \{ h \mapsto S \mid \texttt{init\_handler } h \ \dots \text{ end} \in \widetilde{I},\ \texttt{@st } \{ h_{K},\ S \} \in \widetilde{K},\ h=h_{K} \} \\
&\Delta &&= \quad \Delta_H \cup \Delta_I
\end{align}
$$


## Value Typing

$$
\Large \Psi;\ \Delta;\ \Gamma \vdash w : A
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
	c \text{ has base type } C
}{
\Psi;\ \Delta;\ \Gamma \vdash c:C
}\quad &(\text{TV-Literal})
\\ \\
&\frac{
	\Psi(f_{n}) = f(A_{i},\dots,A_{n}) \to B
}{
	\Psi;\ \Delta;\ \Gamma \vdash f_{n} : f(A_{i},\dots,A_{n}) \to B
} \quad &(\text{TV-Func}) 
\\ \\
&\frac{
	\Delta(h) = S^?
}{
	\Psi;\ \Delta;\ \Gamma \vdash h : \text{Handler}(S^?)
} \quad &(\text{TV-Handler})
\\ \\
&\frac{
	\Delta(h) = S
}{
	\Psi;\ \Delta;\ \Gamma \vdash h : \text{InitHandler}(S)
} \quad &(\text{TV-InitHandler}) 
\end{align}
$$

> [!question] 
> Is that how I would access the handler environment?


## Computation Typing

$$
\Large \Psi; \ \Delta; \ \Gamma  \mid S_1 \rhd e : A \lhd S_2
$$

$$
\begin{align}
&\frac{
    \Psi;\ \Delta;\ \Gamma \mid S_1 \rhd M : A \lhd S_1 \quad \quad \Psi;\ \Delta;\ \Gamma, x:A \mid S_1 \rhd N : B \lhd S_2 \\
}{
    \Psi;\ \Delta;\ \Gamma \mid S_1 \rhd x = M; N: B \lhd S_2 \\
} \quad &(\text{T-Let})
\\ \\
&\frac{
    \Gamma \vdash v:A
}{
    \Psi;\ \Delta;\ \Gamma \mid S \rhd e: A \lhd S
} \quad &(\text{T-Pure})
\\ \\
&\frac{
    (\Gamma \vdash v_{i} : A_{i})_{i \in 1 \dots n} \quad \quad \Psi(f_{n}) = f(A_{1},\dots,A_{n}) : B \\
}{
    \Psi;\ \Delta;\ \Gamma \mid S \rhd f(v_{1}, \dots,v_{n}) : B \lhd S \\
} \quad &(\text{T-App})
\\ \\
&\frac{
    \begin{array}{c}
        \Gamma \vdash w : A \\
        \forall i \in I.\ ( \vdash p_i : A \implies \Gamma_i \;\land\; \Delta;\ \Gamma, \Gamma_i \mid S_0 \rhd N_i : B_i \lhd S'_{i} ) \\
        (B, S_2) = \bigsqcup_{i \in I} (B_i, S'_{i}) \\
    \end{array}
}{
    \Psi;\ \Delta;\ \Gamma \mid S_0 \rhd \texttt{case } w \texttt{ do } \{p_i \to N_i\}_{i \in I} \text{ end} : B \lhd S_2 \\
} \quad &(\text{T-Case})
\\ \\
&\frac{
    \Gamma \vdash w: \text{ActorState}(A)
}{
    \Psi;\ \Delta;\ \Gamma \mid S \rhd \texttt{getState}(w) : A \lhd S
} \quad &(\text{T-Get})
\\ \\
&\frac{
    \Gamma \vdash v:A \quad \Gamma \vdash w: \text{ActorState}(A)
}{
    \Psi;\ \Delta;\ \Gamma \mid S \rhd \texttt{setState}(v,w) : \text{ActorState}(A) \lhd S
} \quad &(\text{T-Set})
\\ \\
&\frac{
    \Gamma \vdash v_{s}\!:\!\text{SessionCtx} \quad \Gamma \vdash V\!:\!A_j \quad j \in I \\
}{ \\
    \Psi;\ \Delta;\ \Gamma \mid \text{p} \oplus \{ l_i(A_i).S_i \}_{i \in I} \rhd \texttt{maty\_send}(v_{s}, \text{p}, \{l_{j}, V\}) : \text{Atom} \lhd S_j \\
} \quad &(\text{T-Send})
\\ \\
&\frac{
    \Delta(h) = S^h \quad \quad \Gamma \vdash e : \text{ActorState}(A)
}{ 
    \Psi;\ \Delta;\ \Gamma \mid S^h \rhd \texttt{maty\_suspend} \ h : \bot \lhd \bot
}\quad &(\text{T-Suspend})
\\ \\
&\frac{
    \Gamma \vdash e : \text{ActorState}(A) \\
}{
    \Psi;\ \Delta;\ \Gamma \mid S \rhd \texttt{maty\_{done}} \ e : \text{ActorState}(A) \lhd S^{\text{end}} \\
} \quad &(\text{T-Done})
\end{align}
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
    \Delta(h) = S \\
    \vdash p : A_{\text{msg}} \implies \Gamma_{msg} \\
    \vdash p_{\sigma} : \text{ActorState}(B) \implies \Gamma_{\sigma} \\ \\
    \text{dom}(\Gamma_{\text{msg}}) \cap \text{dom}(\Gamma_{\sigma}) = \emptyset \\
    \Gamma_{args} = \Gamma_{\text{msg}} \cup \Gamma_{\sigma} \\ \\
    \Delta;\ \Psi, \Gamma_{args} \mid S \rhd e : B' \lhd S' \\
(B' = \text{ActorState}(C) \land S' = S^{\text{end}}) \lor (B' = \bot \land S' = \bot) \\
 \end{array}
}{
    \Delta; \Psi \vdash ( \texttt{handler } h,\ \text{q},\ \{ l, p_{\text{msg}} :: A_{\text{msg}}\},\ p_{\sigma} \texttt{ do } e \texttt{ end}) \text{ ok}
}
\quad (\text{WF-Handler})
$$

$$
\frac{
 \begin{array}{l}
    \Delta(h) = S \\
    \forall i \in 1..n \quad \left( \vdash p_i : A_i \implies \Gamma_i \right) \\
    \forall i \neq j . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset \\
    \Gamma_{p} = \bigcup_{i=1..n} \Gamma_i \\ \\
    \vdash p_{\sigma} : \text{ActorState}(B) \implies \Gamma_{\sigma} \\
    \text{dom}(\Gamma_{p}) \cap \text{dom}(\Gamma_{\sigma}) = \emptyset \\
    \Gamma_{args} = \Gamma_{p} \cup \Gamma_{\sigma} \\ \\
    \Delta;\ \Psi, \Gamma_{args} \mid S \rhd e : B' \lhd S' \\
    B' = \bot \land S' = \bot \\
 \end{array}
}{
    \Delta; \Psi \vdash ( \texttt{init\_handler } h,\ \{ p_1 :: A_{1}, \dots, p_n :: A_{n}\},\ p_{\sigma} \texttt{ do } e \texttt{ end}) \text{ ok}
}
\quad (\text{WF-InitHandler})
$$


