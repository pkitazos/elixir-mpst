---
tags:
  - v2
---
# Elixir Syntax

$$
\begin{align}
&\text{Variables} &x \\
&\text{Functions} &f_n \\
&\text{Handler} &h &::= \text{atom}\\
\\
% should be 0 or 1 InitHandlers, not any number
&\text{Module} &M &::= \texttt{defmodule} \ m \  \texttt{do} \ \widetilde{I} \ \widetilde{K} \ \widetilde{H} \ \widetilde{F} \ \texttt{end}\\
&\text{Session Annotation} &K &::= \texttt{@st} \ \{h, S\} \\
&\text{InitHandler} &I &::= \texttt{@init\_handler} \ h \ F \\
&\text{Handler} &H &::= \texttt{@handler} \ h \ F \\
&\text{Function} &F &::= \texttt{@spec} \ f(\widetilde{A}) :: B \ \texttt{def} \ f \  \texttt{do} \ t \ \texttt{end}\\
\\
&\text{Basic Values} &b &::= \text{atom} \mid \text{nil} \mid \text{boolean} \mid \text{number} \mid \text{binary} \mid \text{date} \mid \text{PID} \mid \text{Ref} \mid [] \\
&\text{Values} &v &::= b \mid [v_1 \mid v_2] \mid \{v_1, \ \dots, \ v_n\} \mid \texttt{\%}\{ (v_i \ \texttt{=>} \ v_i)_{i \in I} \} \\
\\
&\text{Identifiers} &w &::= b \mid x \\
&\text{Patterns} &p &::= v \mid x \\
\\
&\text{Expressions} &e &::= p \\ &&&\mid w_1 \ \diamond \ w_2 \mid \texttt{not} \ w \\
&&&\mid x=e_1;e_2 \\
&&&\mid f(w_1, \ \dots, \ w_n) \\
&&&\mid \texttt{case} \ e \ \texttt{do} \ (p_i \rightarrow e_i)_{i \in I} \ \texttt{end} \\
&&&\mid \texttt{maty\_send}(\text{SessionCtx},\text{Role},\{\text{atom}, v\}) \\
&&&\mid \texttt{maty\_suspend} \ h \\
&&&\mid \texttt{maty\_end} \ \text{ActorState}(A) \\
\\
&\text{Binary Operators} &\diamond &::=  \texttt{<} \mid \texttt{>} \mid \texttt{<=}  \mid \texttt{>=} \mid \texttt{==} \mid \texttt{!=} \mid \texttt{+} \mid \texttt{-} \mid \texttt{*} \mid \texttt{/} \mid \texttt{<>} \mid \texttt{and} \mid  \texttt{or}
\end{align}
$$

>[!question]
> 1. I think $w$, $v$ and $b$ are confusing me a little, in terms of what can go where. 
> 	- $b$ is just literal values + the empty list
> 	- $v$ is literal values + empty list + tuples, lists, and maps
> 	- $w$ is literal values + variables (so does that mean I can't have variables in tuples, lists, maps?)
> 	- $p$ is literal values +  empty list + tuples, lists, and maps + variables (so everything except still I can't have variables in tuples, lists, maps)
> 	Obviously that's too restrictive I should be able to built up a tuple, list, map using variables. But I'm not sure what I need to change to allow that.
> 
> 2. Not sure if the `maty_*` definitions are correct.
> 	- Is that how I should specify that the `maty_send` function takes some session context, role and that messages have atoms for labels?
> 	- Same question for the `maty_end` function, do I specify the return type there? or do I just say that it's an identifier $w$?
> 
> 3. Is the way I'm defining Basic Values (above) and Basic Types (below) wrong? It seems like I'm ending up with a lot of duplication and I think it's because I'm a little unclear about what the distinction is.


## Types
$$
\begin{align}
& &\text{Maybe}(A) &::= A \mid \bot_{T} \\
&\text{Base Types} &C &::= \text{Atom} \mid \text{Nil} \mid \text{Bool} \mid \text{Number} \mid \text{Binary} \mid \text{Date} \mid \text{PID} \mid \text{Ref} \mid \text{Date} \\
&\text{Types} &A, B &::= C \\ 
&&&\mid \text{List}[A] \mid \text{Tuple}[A_1, \dots, A_n] \mid \text{Map}[C, A] \\
&&&\mid f(A_1,\dots,A_n) \rightarrow B \\ 
&&&\mid \text{Handler}(S^?) \\ 
&&&\mid \text{InitHandler}(S) \\ 
\end{align}
$$

> [!question] 
> I couldn't think of what other symbol to use, is it okay to use $\text{Maybe}(A)$ like that?

## Session Types
$$
\begin{align}
\text{Maybe}(S) &::= S \mid \bot_{S} \\
\\
S &::= S^{\text{end}} \mid S^! \mid S^? \mid h \\

S^{\text{end}} &::= \text{end} &&(\text{Protocol finished successfully}) \\

S^! &::= \oplus p:\{l_i(A_i).S_i\}_{i \in I} &&(\text{Internal Choice: Send } l_j(A_j) \text{ to } p\text{, continue as } S_j) \\

S^? &::= \& p:\{l_i(A_i).S_i\}_{i \in I} &&(\text{External Choice: Expect } l_j(A_j) \text{ from } p\text{, continue as } S_j) \\

S^{h} &::= h &&(\text{Continue protocol at handler } h\text{'s session type}) \\
\end{align}
$$

>[!question]
>What do I need to include along with $h$ here to make it clear that it's a label which is used to lookup a session type in $\Delta$


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
	T \sqcup T &= T \\
	\bot \sqcup T &= T \\
	T \sqcup \bot &= T \\
	\bot \sqcup \bot &= \bot \\
\end{align}
$$

**For Session Type - Type Pairs**
$$
(S_a, \ T_a) \ \bigsqcup \ (S_b, \ T_b) = (S_a \sqcup S_b, \ T_a \sqcup T_b)
$$

---


# Environments

$$
\begin{align} \\
\Gamma &= \cdot \mid \Gamma, x:A \\
\\
\Delta &= \{ \ h \mapsto S \mid \texttt{@handler } h \ F \in \widetilde{H} \ \} \\ \\
\Psi &= \{ \ f_{n} \mapsto f(A_{i}, \dots, A_{n}) \to B \mid \texttt{@spec} \ f(A_{i}, \dots,A_{n}) :: B \ \texttt{def} \ f \  \texttt{do} \ t \ \texttt{end} \in \widetilde{F} \ \}
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
\Large
\begin{align}

\end{align}
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
    \Psi;\ \Delta;\ \Gamma \mid S \rhd \texttt{maty\_{end}} \ e : \text{ActorState}(A) \lhd S^{\text{end}} \\
} \quad &(\text{T-End})
\end{align}
$$


## Well-Formedness

**Everything inlined**

$$
\begin{align}
&\frac{
  \begin{array}{l}
  \forall i \in 1..n \quad \left( \vdash p_i : A_i \implies \Gamma_i \right) \\
  \forall i \neq j . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset \\
  \Gamma_{args} = \bigcup_{i=1..n} \Gamma_i \\
  \Psi, \Gamma_{args} \vdash e : B' \\
  B' <: B
  \end{array}
}{
  \Psi \vdash (\texttt{@spec } f(A_1, \dots, A_n) :: B \texttt{ def } f(p_1, \dots, p_n) \texttt{ do } e \texttt{ end}) \text{ ok}
}
\quad &(\text{WF-Func})
\\ \\
&\frac{
  \begin{array}{l}
    \Delta(h) = S \\
    \forall i \in 1..n \quad \left( \vdash p_i : A_i \implies \Gamma_i \right) \\
    \forall i \neq j . \quad \text{dom}(\Gamma_i) \cap \text{dom}(\Gamma_j) = \emptyset \\
    \Gamma_{args} = \bigcup_{i=1..n} \Gamma_i \\
    \Delta; \Psi, \Gamma_{args} | S \rhd e : B' \lhd S' \\
    B' <: B \\
    S' <: S^{\text{end}} \\
  \end{array}
}{
  \Delta; \Psi \vdash ( \texttt{@handler } h \texttt{ @spec } f(A_1, \dots, A_n) :: B \texttt{ def } f(p_1, \dots, p_n) \texttt{ do } e \texttt{ end}) \text{ ok}
}
&(\text{WF-Handler})
\end{align}
$$

> [!note]
> I can remove subtyping and just require that the types match exactly if that makes my life a lot harder in the implementation.



**With BindParams helper**

$$
\begin{align}
&\frac{
  \begin{array}{l}
    \text{BindParams}((p_1, \dots, p_n), (A_1, \dots, A_n)) \implies \Gamma_{args} \\
    \Psi, \Gamma_{args} \vdash e : B' \\
    B' <: B
  \end{array}
}{
    \Psi \vdash (\texttt{@spec } f(A_1, \dots, A_n) :: B \texttt{ def } f(p_1, \dots, p_n) \texttt{ do } e \texttt{ end}) \text{ ok}
}
&(\text{WF-Func})
\\ \\
&\frac{
  \begin{array}{l}
    \Delta(h) = S \\
    \text{BindParams}((p_1, \dots, p_n), (A_1, \dots, A_n)) \implies \Gamma_{args} \\
    \Delta; \Psi, \Gamma_{args} | S \rhd e : B' \lhd S' \\
    B' <: B \\
    S' <: S^{\text{end}} \\
  \end{array}
}{
    \Delta; \Psi \vdash ( \texttt{@handler} \ h \ \ \texttt{@spec } f(A_1, \dots, A_n) :: B \texttt{ def } f(p_1, \dots, p_n) \texttt{ do } e \texttt{ end}) \text{ ok}
}
&(\text{WF-Handler})
\end{align}
$$

$$
\begin{align}
&\frac{
    p=[] \quad A=[] \\
}{
    \text{BindParams}(p, A) \implies \cdot
} &(\text{Params-0})
\\ \\
&\frac{
    \vdash p : A \implies \Gamma_p \quad
    \text{BindParams}(p_{rest}, A_{rest}) \implies \Gamma_{rest} \quad
    \text{dom}(\Gamma_p) \cap \text{dom}(\Gamma_{rest}) = \emptyset
}{
    \text{BindParams}([p \mid p_{rest}], [A \mid A_{rest}]) \implies (\Gamma_p \cup \Gamma_{rest})
} &(\text{Params-N})
\end{align}
$$
