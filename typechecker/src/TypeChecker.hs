module TypeChecker where

import Control.Monad (unless)
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Types

-- utils

err :: (Show t) => t -> t -> String -> String
err got expected msg = unlines [msg, "  Expected: " ++ show expected, "  Found:    " ++ show got]

guard :: (Show t, Eq t) => t -> t -> String -> Either String ()
guard a b s = unless (a == b) $ Left (err a b s)

requireExprTy :: String -> Maybe (ty, st) -> Either String (ty, st)
requireExprTy ctx Nothing = Left $ "Expected a return type and postcondition in: " ++ ctx ++ ", but got suspend."
requireExprTy _ (Just (x, y)) = Right (x, y)

allowSuspend :: Maybe (ty, st) -> Either String (Maybe (ty, st))
allowSuspend Nothing = Right Nothing
allowSuspend just@(Just (t, s)) = Right just

mergeSuspends :: (Show ty, Eq ty, Show st, Eq st) => Maybe (ty, st) -> Maybe (ty, st) -> Either String (Maybe (ty, st))
mergeSuspends Nothing Nothing = Right Nothing
mergeSuspends Nothing just@(Just _) = Right just
mergeSuspends just@(Just _) Nothing = Right just
mergeSuspends (Just (t1, s1)) (Just (t2, s2))
  | t1 == t2 && s1 == s2 = Right (Just (t1, s1))
  | otherwise = do
      guard t1 t2 "Branches of 'if' must have the same return type."
      guard s2 s1 "Branches of 'if' must have the same session postcondition."
      Left "Branches differ in type or postcondition"

-- checkBranch

checkBranch :: Env -> Participant -> (Label, Name, Type, ST, Expr) -> Either String (Maybe (Label, Type, ST))
checkBranch env participant (l, x, xTy, preST, body) = do
  expr <- tcExpr (Map.insert x xTy env) preST body
  case expr of
    Nothing -> pure Nothing
    Just (retTy, retST) -> do
      guard retTy (Base TUnit) ("Handler branch " ++ l ++ " must return TUnit.")
      guard retST End ("Handler branch " ++ l ++ " must end with session type End.")
      pure (Just (l, xTy, preST))

-- tcVal --

tcVal :: Env -> Val -> Either String Type
-- TV-CONST
tcVal _ VUnit = Right (Base TUnit)
tcVal _ (VBool _) = Right (Base TBool)
tcVal _ (VInt _) = Right (Base TInt)
-- TV-VAR
tcVal env (VVar x) = case Map.lookup x env of
  Just t -> Right t
  Nothing -> Left $ "Unbound variable " ++ show x ++ " in tcVal."
-- TV-LAM
tcVal env (VLam x xTy preST postST m) = do
  m' <- tcExpr (Map.insert x xTy env) preST m
  case m' of
    Nothing -> pure (Func xTy preST Nothing Nothing)
    Just (retTy, retST) -> do
      guard retST postST "Function postcondition mismatch:"
      pure (Func xTy preST (Just postST) (Just retTy))

-- TV-HANDLER
tcVal env (VHandler participant bs) = do
  bs' <- mapM (checkBranch env participant) bs
  pure (HandlerT (InST participant (catMaybes bs')))

-- tcExpr

tcExpr :: Env -> ST -> Expr -> Either String (Maybe (Type, ST))
-- gotta write out a rule for typechecking continuations
tcExpr env st (ECont m n) = do
  m' <- tcExpr env st m

  Left ""

-- T-LET (Nothing wins)
tcExpr env st (ELet x m n) = do
  expr <- tcExpr env st m
  case expr of
    Nothing ->
      -- m suspends, so the entire 'let' also suspends,
      -- because we never get a value for x, nor proceed with N.
      pure Nothing
    Just (retTy, retST) ->
      -- m returned (retTy, retST). Now we can bind x.
      tcExpr (Map.insert x retTy env) retST n

-- T-RETURN
tcExpr env st (EReturn val) = do
  retTy <- tcVal env val
  pure (Just (retTy, st))

-- T-APP
-- ! double check this
tcExpr env st (EApp v w) = do
  -- does W have the same type as paramTy
  -- then the return type of application is: return type of body and postST

  -- unless (st == preST) $ Left (err st preST "Session precondition mismatch for function application.")

  wTy <- tcVal env w
  -- unless (paramTy == wTy) $ Left (err paramTy wTy "Parameter type mismatch in function application.")

  vTy <- tcVal env v
  case vTy of
    (Func _ _ (Just post) (Just b)) -> pure (Just (b, post))
    (Func _ _ (Nothing) (Nothing)) -> pure (Nothing) -- double check this
    _ -> Left (err vTy (Func wTy st Nothing Nothing) "Parameter type mismatch in function application.") -- error message should be able to specify generic type?

-- T-SPAWN
-- if computation M is typeable under type environment Γ with session precondition end
-- and returns unit and post condition end
-- then spawn M has type unit and doesn't affect the session type
tcExpr env st (ESpawn m) = do
  m' <- tcExpr env End m
  case m' of
    Just (mTy, mST) -> do
      guard mTy (Base TUnit) "Spawned computation must return TUnit."
      guard mST End "Spawned computation must end with session type End."
      pure (Just (Base TUnit, st))
    -- type error if this is not typable under End
    Nothing -> Left "can't do this in a spawn expression"

-- T-IF
tcExpr env st (EIf cond m n) = do
  condTy <- tcVal env cond
  guard condTy (Base TBool) "Condition in 'if' must be Bool."

  m' <- tcExpr env st m
  m'' <- allowSuspend m'

  n' <- tcExpr env st n
  n'' <- allowSuspend n'

  mergeSuspends m'' n''

-- T-SEND
tcExpr env (SOut (OutST p branches)) (ESend p' l val) = do
  guard p' p "Message recepient mismatch: "
  valTy <- tcVal env val

  case find (\(l', _, _) -> l' == l) branches of
    Nothing -> Left (err l p ("No label '" ++ l ++ "' found in session type for participant " ++ p))
    Just (_, payloadTy, postST) -> do
      guard payloadTy valTy "Payload type mismatch in send."
      pure (Just (Base TUnit, postST))

-- T-SUSPEND
tcExpr env (SIn inSt) (ESuspend handlerVal) = do
  vTy <- tcVal env handlerVal
  case vTy of
    HandlerT hInSt -> do
      guard hInSt inSt "Handlers "
      pure Nothing
    _ -> Left $ "Expected HandlerT(InST ...), got " ++ show vTy

-- T-NEWAP
tcExpr _ st (ENewAP ps) =
  pure $ Just (APType ps, st)
-- T-REGISTER
tcExpr env st (ERegister v p m) = do
  vTy <- tcVal env v
  case vTy of
    APType ps ->
      case find (\(p', _) -> p' == p) ps of
        Nothing -> Left $ "Participant " ++ p ++ " not found in access point."
        Just (_, t) -> do
          m' <- tcExpr env t m
          (mTy, mST) <- requireExprTy "suspends" m'

          guard mTy (Base TUnit) "Registered callback must return TUnit."
          guard mST End "Registered callback must end with session type End."

          pure $ Just (Base TUnit, st)
    _ -> Left (err vTy (APType []) "Expected an APType in register.")

-- catchall
tcExpr _ _ other = Left $ "Invalid expression (or partial rule coverage): " ++ show other
