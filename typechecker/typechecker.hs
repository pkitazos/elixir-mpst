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
      unless (t1 == t2) $ Left (err t1 t2 "Branches of 'if' must have the same return type.")
      unless (s1 == s2) $ Left (err s2 s1 "Branches of 'if' must have the same session postcondition.")
      Left "Branches differ in type or postcondition"

-- checkBranch

checkBranch :: Env -> Participant -> (Label, Name, Type, ST, Expr) -> Either String (Maybe (Label, Type, ST))
checkBranch env participant (l, x, xTy, preST, body) = do
  expr <- tcExpr (Map.insert x xTy env) preST body
  case expr of
    Nothing -> pure Nothing
    Just (retTy, retST) -> do
      unless (retTy == Base TUnit) $ Left (err retTy (Base TUnit) ("Handler branch " ++ l ++ " must return TUnit."))
      unless (retST == End) $ Left (err retST End ("Handler branch " ++ l ++ " must end with session type End."))
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
    Just (retTy, retST) -> do
      unless (retST == postST) $ Left (err retST postST "Function postcondition mismatch:")
      pure (Func xTy preST postST (Just retTy))
    Nothing -> pure (Func xTy preST postST Nothing)

-- TV-HANDLER
tcVal env (VHandler participant bs) = do
  bs' <- mapM (checkBranch env participant) bs
  pure (HandlerT (InST participant (catMaybes bs')))

-- tcExpr

tcExpr :: Env -> ST -> Expr -> Either String (Maybe (Type, ST))
-- T-LET
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
tcExpr env st (EApp (VLam paramName paramTy preST postST body) w) = do
  unless (st == preST) $ Left (err st preST "Session precondition mismatch for function application.")

  wTy <- tcVal env w
  unless (paramTy == wTy) $ Left (err paramTy wTy "Parameter type mismatch in function application.")

  body' <- tcExpr (Map.insert paramName paramTy env) preST body
  case body' of
    Just (retTy, retST) -> do
      unless (retST == postST) $ Left (err retST postST "Session postcondition mismatch after function application.")
      pure (Just (retTy, retST))
    Nothing -> pure Nothing -- ?? if we apply a value to a function that suspends, does that mean there's no session postcondition?

-- T-SPAWN
tcExpr env st (ESpawn m) = do
  m' <- tcExpr env End m
  case m' of
    Just (mTy, mST) -> do
      unless (mTy == Base TUnit) $ Left (err (Base TUnit) mTy "Spawned computation must return TUnit.")
      unless (mST == End) $ Left (err End mST "Spawned computation must end with session type End.")
      pure $ Just (Base TUnit, st)
    Nothing -> pure $ Just (Base TUnit, st) -- if the spawn computation suspends, just return TUnit and the precondition unchanged

-- T-IF
tcExpr env st (EIf cond m n) = do
  condTy <- tcVal env cond
  unless (condTy == Base TBool) $ Left (err (Base TBool) condTy "Condition in 'if' must be Bool.")

  m' <- tcExpr env st m
  m'' <- allowSuspend m'

  n' <- tcExpr env st n
  n'' <- allowSuspend n'

  mergeSuspends m'' n''

-- T-SEND
tcExpr env (SOut (OutST p branches)) (ESend p' l val) = do
  unless (p == p') $ Left (err p' p "Message recepient mismatch: ")
  valTy <- tcVal env val

  case find (\(l', _, _) -> l' == l) branches of
    Nothing -> Left (err l p ("No label '" ++ l ++ "' found in session type for participant " ++ p))
    Just (_, payloadTy, postST) -> do
      unless (payloadTy == valTy) $ Left (err valTy payloadTy "Payload type mismatch in send.")
      pure $ Just (Base TUnit, postST)

-- T-SUSPEND
tcExpr env (SIn inSt) (ESuspend handlerVal) = do
  vTy <- tcVal env handlerVal
  case vTy of
    HandlerT hInSt -> do
      unless (inSt == hInSt) $ Left (err hInSt inSt "Handlers ")
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

          unless (mTy == Base TUnit) $ Left (err mTy (Base TUnit) "Registered callback must return TUnit.")
          unless (mST == End) $ Left (err mST End "Registered callback must end with session type End.")

          pure $ Just (Base TUnit, st)
    _ -> Left (err vTy (APType []) "Expected an APType in register.")

-- catchall
tcExpr _ _ other = Left $ "Invalid expression (or partial rule coverage): " ++ show other
