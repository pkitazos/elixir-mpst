# typechecker

## Syntax of types and type environments

Let's define some aliases first
```haskell
-- Aliases
type Participant = String
type Label = String
type Name = String
```

**Output Session Types**
$$
S^! ::= \text{p} \ \oplus \ \{ l_{i}(A_{i}).S_{i}\}_{i} \\
$$

```haskell
data OutputST = OutST Participant [(Label, Type, ST)]
  deriving (Eq, Show)
```


**Input Session Types**
$$
S^! ::= \text{p} \ \& \ \{ l_{i} (A_{i}).S_{i}\}_{i}
$$

```haskell
data InputST = InST Participant [(Label, Type, ST)]
  deriving (Eq, Show)
```


**Session Types**
$$
S, T ::=  S^! \ | \ S^? \ | \ end
$$
```haskell
-- Session types
--
-- S (session types) can be:
--   end
--   p ⊕ {l_i(A_i).S_i}_i  (Out or Send)
--   p & {l_i(A_i).S_i}_i   (In or Recv)

data ST
  = End
  | SOut OutputST
  | SIn InputST
  deriving (Eq)
```


**Base Types**
$$
C ::= 1 \mid \text{Bool} \mid \text{Int}
$$
```haskell
data BaseType
  = TUnit
  | TInt
  | TBool
  deriving (Eq, Show)
```


**Types**
$$
A, B ::= C \mid A \overset{S,T}{\longrightarrow} B \mid \text{AP}((p_i : S_i)_i) \mid \text{Handler}(S^?)
$$
```haskell
-- Full types (A, B) in Maty
--
-- A type can be:
--   C                 (base)
--   A -(S, T)→ B      (a function type with session pre/post conditions S,T)
--   AP((p_i:S_i)_i)   (access point type)
--   Handler(S?)       (handler type, where S? is an input session type)
--
data Type
  = Base BaseType
  | Func Type ST ST (Maybe Type)
  | APType [(Participant, ST)]
  | HandlerT InputST
  deriving (Eq, Show)
```


**Type Environments**
$$
\Gamma ::= \ \cdot \mid x : A
$$

```haskell
-- Type Environment (Γ)
type Env = Map String Type
```



And next let's define values and expressions for our typechecker implementation.

```haskell
-- Values (V)
data Val
  = VVar Name
  | VUnit
  | VBool Bool
  | VInt Int
  | VLam --  λx.M  with pre= S, post = T
      { lamParam :: Name,
        lamParamType :: Type, -- type of the parameter "x"
        lamPre :: ST, -- session precondition
        lamPost :: ST, -- session postcondition
        lamBody :: Expr -- body M
      }
  | VHandler -- handler p { l_i(x_i) -> M_i }
      { handlerParticipant :: Participant,
        handlerBranches :: [(Label, Name, Type, ST, Expr)]
      }
  deriving (Eq, Show)

-- Expressions / Computations (M)
data Expr
  = EReturn Val
  | ELet Name Expr Expr -- let x <== M in N
  | EApp Val Val -- V W
  | EIf Val Expr Expr -- if V then M else N
  | ESpawn Expr -- spawn M
  | ESend Participant Label Val -- p ! l(V)
  | ESuspend Val -- suspend V
  | ENewAP [(Participant, ST)] -- newAP((p_i : S_i)_i)
  | ERegister Val Participant Expr
  deriving (Eq, Show)
```



And our typechecker will then essentially be comprised of two functions

```haskell
tcVal :: Env -> Val -> Either String Type

tcExpr :: Env -> ST -> Expr -> Either String (Maybe (Type, ST))
```

Where we'll be using the `Either` monad to capture type errors with our `Left` constructor and computed types in our `Right` constructor.

One thing to note is that our `tcExpr` function returns a `Maybe` type as expressions may suspend in which case the return type would be `Nothing`.


## Value Typing

**TV-VAR**
$$
\frac{x : A \in \Gamma}{\Gamma \vdash x : A}
$$

Under type environment $\Gamma$, $x$ has type $A$ if I can type $x$ with type $A$ in $\Gamma$

```haskell
tcVal env (VVar x) = case Map.lookup x env of
  Just t -> Right t
  Nothing -> Left $ "Unbound variable " ++ show x ++ " in tcVal."
```


**TV-CONST**
$$
\frac{c \text{ has base type } C}{\Gamma \vdash c : C}
$$

The constant $c$ has type $C$ under type environment $\Gamma$, given that $c$ is a constant with base type $C$.

```haskell
tcVal _ VUnit = Right (Base TUnit)
tcVal _ (VBool _) = Right (Base TBool)
tcVal _ (VInt _) = Right (Base TInt)
```


**TV-LAM**
$$
\frac{\Gamma, x : A \mid  S \rhd M : B \lhd T}{\Gamma \vdash \lambda x.M : A \overset{S,T}{\longrightarrow} B}
$$

If $M$ is well-typed as a computation of type $B$ when $x$ is available as type $A$ and the session transitions from $S$ to $T$, then $\lambda x.\,M$ is a function from $A$ to $B$ (with session precondition $S$ and postcondition $T$).

```haskell
tcVal env (VLam x xTy preST postST m) = do
  m' <- tcExpr (Map.insert x xTy env) preST m
  case m' of
    Just (retTy, retST) -> do
      unless (retST == postST) $ Left (err retST postST "...")
      pure (Func xTy preST postST (Just retTy))
    Nothing -> pure (Func xTy preST postST Nothing)
```


**TV-HANDLER**
$$
\frac{(\Gamma, x : A_i \mid S_i \rhd M_i : 1 \lhd \text{end})_i}{\Gamma \vdash \texttt{handler} \ \text{p} \{l_i(x_i) \mapsto M_i\}_i : \text{Handler}(p \& \{l_i(A_i).S_i\}_i))}
$$

$\texttt{handler} \ \text{p} \{l_i(x_i) \mapsto M_i\}_i$ the handler is typable with type $\text{Handler}(p \& \{l_i(A_i).S_i\}_i))$ if each continuation $M_i$ is typable with session precondition $S_i$ where the environment is extended with $x_i$ of type $A_i$, and all branches have the postcondition $end$.

```haskell
tcVal env (VHandler participant bs) = do
  bs' <- mapM (checkBranch env participant) bs
  pure (HandlerT (InST participant (catMaybes bs')))


checkBranch ::
	Env ->
	Participant ->
	(Label, Name, Type, ST, Expr) ->
	Either String (Maybe (Label, Type, ST))
checkBranch env participant (l, x, xTy, preST, body) = do
  expr <- tcExpr (Map.insert x xTy env) preST body
  case expr of
    Nothing -> pure Nothing
    Just (retTy, retST) -> do
      unless (retTy == Base TUnit) $ Left (err retTy (Base TUnit) "...")
      unless (retST == End) $ Left (err retST End "...")
      pure (Just (l, xTy, preST))

```


**T-LET**
$$
 \frac{
	\begin{align}
		\Gamma \mid S_1 \rhd M : A \lhd S_2 \\
		\Gamma, x : A \mid S_2 \rhd N : B \lhd S_3
	\end{align}
}{
	\Gamma \mid \text{let } x \Leftarrow  M \ \text{in} \ N : B \lhd S_3
}
$$

```haskell
tcExpr env st (ELet x m n) = do
  expr <- tcExpr env st m
  (retTy, retST) <- requireExprTy "..." expr
  tcExpr (Map.insert x retTy env) retST n
```


**T-RETURN**
$$
\frac{\Gamma \vdash V : A}{\Gamma \mid S \rhd \text{return } V : A \lhd S}
$$

```haskell
tcExpr env st (EReturn val) = do
  retTy <- tcVal env val
  pure (Just (retTy, st))
```


**T-APP**
$$
\frac{
	\begin{align}
		\Gamma \vdash V : A \overset{S,T}{\longrightarrow} B \\
		\Gamma \vdash W : A
	\end{align}
}{\Gamma \mid S \rhd V \ W : B \lhd T}
$$

A function application `V W` is typable provided that the precondition in the function type matches the current precondition, and advances the postcondition to that of the function type.

```haskell
tcExpr env st (EApp (VLam paramName paramTy preST postST body) w) = do
  unless (st == preST) $ Left (err st preST "...")

  wTy <- tcVal env w
  unless (paramTy == wTy) $ Left (err paramTy wTy "...")

  body' <- tcExpr (Map.insert paramName paramTy env) preST body
  case body' of
    Just (retTy, retST) -> do
      unless (retST == postST) $ Left (err retST postST "...")
      pure (Just (retTy, retST))
    Nothing -> pure Nothing
```


**T-SPAWN**
$$
\frac{\Gamma \mid \text{end} \rhd M : 1 \lhd \text{end}}{\Gamma \mid S \rhd \text{spawn } M : 1 \lhd S}
$$

The `spawn M` term spawns a new actor that evaluates term `M`; this rule types the expression with the unit type if the spawned thread `M` must has return type `1` and pre- and postconditions `end` (since the spawned computation is not yet in a session and so cannot communicate)

```haskell
tcExpr env st (ESpawn m) = do
  m' <- tcExpr env End m
  case m' of
    Just (mTy, mST) -> do
      unless (mTy == Base TUnit) $ Left (err (Base TUnit) mTy "...")
      unless (mST == End) $ Left (err End mST "...")
      pure $ Just (Base TUnit, st)
    Nothing -> pure $ Just (Base TUnit, st)
```


**T-IF**
$$
\frac{
\begin{align}
	\Gamma \vdash V : \text{Bool} \\
	\Gamma \mid S_1 \rhd M : A \lhd S_2 \ \ \ \ \Gamma \mid S_1 \rhd N : A \lhd S_2
\end{align}
}{
	\Gamma \mid S_1 \rhd \text{if} \ V \ \text{then} \ M \ \text{else} \ N : A \lhd S_2
}
$$

```haskell
tcExpr env st (EIf cond m n) = do
  condTy <- tcVal env cond
  unless (condTy == Base TBool) $ Left (err (Base TBool) condTy "...")
  m' <- tcExpr env st m
  m'' <- allowSuspend m'
  n' <- tcExpr env st n
  n'' <- allowSuspend n'
  mergeSuspends m'' n''
```


**T-SEND**
$$
\frac{j \in I \ \ \ \Gamma \vdash V : A_i}{
\Gamma \mid \text{p} \ \oplus \ \{l_i(A_i).S_i\}_{i \in I} \rhd \text{p} \ ! \ l_j(V) : 1 \lhd S_j
}
$$

This rule types a send computation `p ! l(V)` if `l` is contained within the selection session precondition, and if `V` has the corresponding type; the postcondition is the session continuation for the specified branch

```haskell
tcExpr env (SOut (OutST p branches)) (ESend p' l val) = do
  unless (p == p') $ Left (err p' p "...")
  valTy <- tcVal env val
  case find (\(l', _, _) -> l' == l) branches of
    Nothing -> Left (err l p ("..."))
    Just (_, payloadTy, postST) -> do
      unless (payloadTy == valTy) $ Left (err valTy payloadTy "...")
      pure $ Just (Base TUnit, postST)
```


**T-SUSPEND**
$$
\frac{\Gamma \vdash V :\text{Handler}(S^?)}{\Gamma \mid S^? \rhd \text{suspend } V : A \lhd S'}
$$

When an actor wishes to receive a message, it must suspend itself and install a message handler using `suspend V`. This rule states that `suspend V` is typable if the handler is compatible with the current session type precondition; since the computation does not return, it can be given an arbitrary return type and postcondition.

```haskell
tcExpr env (SIn inSt) (ESuspend handlerVal) = do
  vTy <- tcVal env handlerVal
  case vTy of
    HandlerT hInSt -> do
      unless (inSt == hInSt) $ Left (err hInSt inSt "...")
      pure Nothing
    _ -> Left $ "..." ++ show vTy

```


**T-NEWAP**
$$
\frac{\phi \text{ is a safety property } \ \ \phi((p_i : T_i)_{i \in I}) \quad}{\Gamma \mid S \rhd \text{newAP}((p_i : T_i)_{i \in I}) : \text{AP}((p_i : T_i)_{i \in I}) \lhd S}
$$

Sessions are initiated using access points: we create an access point for a session with roles and types `(p_i : s_i)_i` using `newAP(p_i:s_i)_i`, which must annotated with the set of roles and local types to be involved in the session. The rule ensures that the session types satisfy a safety property; not implemented for now, but at a high level, if a set of session types is safe then the types are guaranteed never to cause a runtime type error due to a communication mismatch.

```haskell
tcExpr _ st (ENewAP ps) =
  pure $ Just (APType ps, st)
```


**T-REGISTER**
$$
\frac{j \in I \ \ \ \Gamma \vdash V : \text{AP}((\text{p}_i : T_i)_{i \in I}) \ \ \ \Gamma \mid T_j \rhd M : 1 \lhd \text{end} \quad}{\Gamma \mid S \rhd \text{register } V \ \text{p}_j \ M : 1 \lhd S}
$$

An actor can register to take part in a session as role `p` on access point `V` using `register V p M`; term `M` is a callback to be invoked once the session is established. This rule ensures that the access point must contain a session type `T` associated with role `p`, and since the initiation callback will be evaluated when the session is established, `M` must be typable under session type `T`. Since neither newAP nor register perform any communication, the session types are unaltered.

```haskell
tcExpr env st (ERegister v p m) = do
  vTy <- tcVal env v
  case vTy of
    APType ps ->
      case find (\(p', _) -> p' == p) ps of
        Nothing -> Left $ "..."
        Just (_, t) -> do
          m' <- tcExpr env t m
          (mTy, mST) <- requireExprTy "..." m'
          unless (mTy == Base TUnit) $ Left (err mTy (Base TUnit) "...")
          unless (mST == End) $ Left (err mST End "...")
          pure $ Just (Base TUnit, st)
    _ -> Left (err vTy (APType []) "...")
```
