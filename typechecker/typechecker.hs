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
tcVal env (VVar x) =
  case Map.lookup x env of
    Just t -> Right t
    Nothing -> Left $ "Unbound variable " ++ show x ++ " in tcVal."
-- TV-LAM
tcVal env (VLam paramName paramT preST postST body) = do
  (retTy, retST) <- tcExpr (Map.insert paramName paramT env) preST body
  unless (retST == postST) $
    Left $
      unlines
        [ "Function postcondition mismatch:",
          "  Expected: " ++ show postST,
          "  Found:    " ++ show retST
        ]
  pure (Func paramT preST postST retTy)

-- TV-HANDLER
tcVal env (VHandler participant branches) = do
  -- For each branch, typecheck its body under the environment + parameter binding.
  -- Must always return TUnit and session postcondition == End.
  typedBranches <- mapM (checkBranch env participant) branches
  -- If they all succeed, build the handler type from those typed branches.
  pure (HandlerT (InST participant typedBranches))

checkBranch :: Env -> Participant -> (Label, Name, Type, ST, Expr) -> Either String (Label, Type, ST)
checkBranch env participant (l, paramName, paramTy, preST, body) = do
  (retTy, retST) <- tcExpr (Map.insert paramName paramTy env) preST body

  unless (retTy == Base TUnit) $
    Left $
      unlines
        [ "Handler branch " ++ l ++ " must return TUnit.",
          "  Found: " ++ show retTy
        ]

  unless (retST == End) $
    Left $
      unlines
        [ "Handler branch " ++ l ++ " must end with session type End.",
          "  Found: " ++ show retST
        ]

  pure (l, paramTy, preST)

tcExpr :: Env -> ST -> Expr -> Either String (Type, ST)
-- T-LET
tcExpr env st (ELet x m n) = do
  (xTy, postX) <- tcExpr env st m
  tcExpr (Map.insert x xTy env) postX n

-- T-RETURN
tcExpr env st (EReturn val) = do
  retTy <- tcVal env val
  pure (retTy, st)

-- T-APP
tcExpr env st (EApp (VLam paramName paramTy preST postST body) w) = do
  wTy <- tcVal env w
  (retTy, retST) <- tcExpr (Map.insert paramName paramTy env) preST body

  unless (st == preST) $
    Left $
      unlines
        [ "Session precondition mismatch for function application.",
          "  Expected: " ++ show preST,
          "  Found:    " ++ show st
        ]

  unless (paramTy == wTy) $
    Left $
      unlines
        [ "Parameter type mismatch in function application.",
          "  Expected: " ++ show paramTy,
          "  Found:    " ++ show wTy
        ]

  unless (retST == postST) $
    Left $
      unlines
        [ "Session postcondition mismatch after function application.",
          "  Expected: " ++ show postST,
          "  Found:    " ++ show retST
        ]

  pure (retTy, retST)

-- ? do I need to handle Tapplication of non-lamnda values

-- T-SPAWN
tcExpr env st (ESpawn m) = do
  -- m must be typable with pre=End and post=End, returning TUnit
  (mTy, mST) <- tcExpr env End m

  unless (mTy == Base TUnit) $
    Left $
      unlines
        [ "Spawned computation must return TUnit.",
          "  Found: " ++ show mTy
        ]

  unless (mST == End) $
    Left $
      unlines
        [ "Spawned computation must end with session type End.",
          "  Found: " ++ show mST
        ]

  pure (Base TUnit, st)

-- T-IF
tcExpr env st (EIf cond m n) = do
  condTy <- tcVal env cond
  unless (condTy == Base TBool) $
    Left $
      unlines
        [ "Condition in 'if' must be Bool.",
          "  Found: " ++ show condTy
        ]

  (mTy, mST) <- tcExpr env st m
  (nTy, nST) <- tcExpr env st n

  unless (mTy == nTy) $
    Left $
      unlines
        [ "Branches of 'if' must have the same return type.",
          "  Branch1: " ++ show mTy,
          "  Branch2: " ++ show nTy
        ]

  unless (mST == nST) $
    Left $
      unlines
        [ "Branches of 'if' must have the same session postcondition.",
          "  Branch1: " ++ show mST,
          "  Branch2: " ++ show nST
        ]

  pure (mTy, mST)

-- T-SEND
tcExpr env (SOut (OutST p branches)) (ESend p' l val) = do
  -- if in one of the st branches there is a label that matches l
  -- and the type of val matches the type of the payload for that branch
  -- then the postcondition is the one of that branch's
  -- and the return type is unit
  unless (p == p') $
    Left $
      unlines
        [ "Session precondition expects to send to participant: " ++ p,
          "  Found: " ++ p'
        ]

  valTy <- tcVal env val
  case find (\(l', _, _) -> l' == l) branches of
    Nothing ->
      Left $ "No label '" ++ l ++ "' found in session type for participant " ++ p
    Just (_, payloadTy, postST) -> do
      unless (payloadTy == valTy) $
        Left $
          unlines
            [ "Payload type mismatch in send.",
              "  Expected: " ++ show payloadTy,
              "  Found:    " ++ show valTy
            ]
      pure (Base TUnit, postST)

-- T-SUSPEND
tcExpr env (SIn s) (ESuspend v) = do
  -- ? do I also need to check the participants here
  vTy <- tcVal env v
  case vTy of
    HandlerT (InST p branches) ->
      -- ? what should the actual return type be here, this seems to be saying that the session ends here
      pure (Base TUnit, End)
    _ ->
      Left $
        unlines
          [ "Suspend requires a HandlerT(InST ...).",
            "  Found: " ++ show vTy
          ]

-- T-NEWAP
tcExpr _ st (ENewAP ps) = do
  -- check safety property
  pure (APType ps, st)

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
          Left $ "Participant " ++ p ++ " not found in access point."
        Just (_, t) -> do
          (mTy, mST) <- tcExpr env t m
          unless (mTy == Base TUnit) $
            Left $
              unlines
                [ "Registered callback must return TUnit.",
                  "  Found: " ++ show mTy
                ]
          unless (mST == End) $
            Left $
              unlines
                [ "Registered callback must end with session type End.",
                  "  Found: " ++ show mST
                ]
          pure (Base TUnit, st)
    _ ->
      Left $
        unlines
          [ "Expected an APType in register.",
            "  Found: " ++ show vTy
          ]

-- catchall
tcExpr _ _ other = Left $ "Invalid expression (or partial rule coverage): " ++ show other
