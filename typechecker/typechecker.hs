module TypeChecker where

import Control.Monad (unless)
import Data.List (find)
import Data.Map qualified as Map
import Types

tcVal :: Env -> Val -> Either String Type
-- TV-CONST
tcVal _ VUnit = Right $ Base TUnit
tcVal _ (VBool _) = Right $ Base TBool
tcVal _ (VInt _) = Right $ Base TInt
-- TV-VAR
tcVal env (VVar x) = do
  case Map.lookup x env of
    Just t -> Right t
    Nothing -> Left $ "Unbound variable in tcVal: " ++ show x

-- TV-LAM
tcVal env (VLam paramName paramT preST postST body) = do
  (retTy, retST) <- tcExpr (Map.insert paramName paramT env) preST body
  unless (retST == postST) $ Left "Post condition does not match"
  Right $ Func paramT preST postST retTy

-- TV-HANDLER
tcVal env (VHandler participant branches) = do
  -- for each branch, typecheck its body under the environment + parameter binding
  -- must always return TUnit and session postcondition == End
  typedBranches <- mapM (checkBranch env participant) branches
  -- if they all succeed, we get a list of (Label, Type, ST).
  -- then we build the handler type from those typed branches.
  Right (HandlerT (InST participant typedBranches))

checkBranch :: Env -> Participant -> (Label, Name, Type, ST, Expr) -> Either String (Label, Type, ST)
checkBranch env participant (l, paramName, paramTy, preST, body) = do
  (retTy, retST) <- tcExpr (Map.insert paramName paramTy env) preST body
  unless (retTy == Base TUnit) $
    Left $
      "Branch " ++ l ++ " must return TUnit, but got type " ++ show retTy

  unless (retST == End) $
    Left $
      "Branch " ++ l ++ " expects session postcondition End, but got type " ++ show retST

  Right (l, paramTy, preST)

tcExpr :: Env -> ST -> Expr -> Either String (Type, ST)
-- T-LET
tcExpr env st (ELet x m n) = do
  (xTy, postX) <- tcExpr env st m
  res <- tcExpr (Map.insert x xTy env) postX n
  Right res

-- T-RETURN
tcExpr env st (EReturn x) = do
  retTy <- tcVal env x
  Right (retTy, st)

-- T-APP
tcExpr env st (EApp (VLam paramName paramTy preST postST body) w) = do
  wTy <- tcVal env w
  (retTy, retST) <- tcExpr (Map.insert paramName paramTy env) preST body

  unless (st == preST) $
    Left $
      "Function expects session precondition "
        ++ show preST
        ++ ", but got "
        ++ show st

  unless (paramTy == wTy) $
    Left $
      "Function requires param of type "
        ++ show paramTy
        ++ ", but got "
        ++ show wTy

  unless (retST == postST) $
    Left $
      "Function expects session postcondition "
        ++ show postST
        ++ ", but got "
        ++ show retST

  Right (retTy, retST)

-- T-SPAWN
tcExpr env st (ESpawn m) = do
  -- m must be typable with pre=End and post=End, returning TUnit
  (mTy, mST) <- tcExpr env End m

  unless (mTy == Base TUnit) $
    Left "Spawned computation must return TUnit"

  unless (mST == End) $
    Left "Spawned computation postcondition must be End"

  Right (Base TUnit, st)

-- T-IF
tcExpr env st (EIf v m n) = do
  vTy <- tcVal env v

  unless (vTy == Base TBool) $
    Left $
      "Expected TBool, got " ++ show vTy

  (mTy, mST) <- tcExpr env st m
  (nTy, nST) <- tcExpr env st n

  unless (mTy == nTy) $
    Left $
      "Return type of " ++ show m ++ " and " ++ show n ++ " must be the same"

  unless (mST == nST) $
    Left $
      "Session postcondition of " ++ show m ++ " and " ++ show n ++ " must be the same"

  Right (mTy, mST)

-- T-SEND
tcExpr env (SOut (OutST p branches)) (ESend p' l val) = do
  -- if in one of the st branches there is a label that matches l
  -- and the type of val matches the type of the payload for that branch
  -- then the postcondition is the one of that branch's
  -- and the return type is unit
  unless (p == p') $
    Left $
      "Session precondition expects next message to be sent to " ++ p ++ " got " ++ p'

  valTy <- tcVal env val

  case find (\(l', _, _) -> l' == l) branches of
    Nothing -> Left $ "No branch in session precondition has label " ++ l
    Just (_l, payloadTy, postST) ->
      if payloadTy /= valTy
        then Left "Payload type and value received don't match"
        else Right (Base TUnit, postST)

-- T-SUSPEND
tcExpr env (SIn s) (ESuspend v) = do
  vTy <- tcVal env v
  -- ? do I also need to check the participants here
  case vTy of
    (HandlerT (InST _ _)) -> Right (Base TUnit, End) -- ! not sure about this retirn type, how do i make it generic ?
    other -> Left $ "Handler must be of HandlerT(InST ...), but got " ++ show other

-- T-NEWAP
tcExpr _ st (ENewAP ps) = do
  -- check safety property
  Right (APType ps, st)

-- T-REGISTER
tcExpr env st (ERegister v p m) = do
  -- vTy must be an access point
  -- the access point must have participant p with an associated session type T
  -- m must be typable given session precondition T
  vTy <- tcVal env v

  case vTy of
    APType ps ->
      case find (\(p', _) -> p' == p) ps of
        Nothing ->
          Left $ "Participant " ++ p ++ " not found in access point"
        Just (_, t) -> do
          (mTy, mST) <- tcExpr env t m

          unless (mTy == Base TUnit) $
            Left "Registered callback must return TUnit"

          unless (mST == End) $
            Left "Registered callback must end with session = End"

          Right (Base TUnit, st)
    _ ->
      Left ("Expected APType, got " ++ show vTy)

-- catchall
tcExpr _ _ _ = Left "Invalid configuration"
