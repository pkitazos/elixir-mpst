module STLC where

import Control.Monad (unless)
-- import Control.Monad.State
import Data.Map (Map, insert, lookup)
import Data.Map qualified as Map
import Data.Set (Set)

type Name = String

-- Base types
data BaseType = TUnit | TInt | TBool deriving (Eq)

instance Show BaseType where
  show TUnit = "1"
  show TInt = "Int"
  show TBool = "Bool"

data Type = TVar String | B BaseType | TArrow Type Type deriving (Eq)

instance Show Type where
  show (B b) = show b
  show (TVar t) = show t
  show (TArrow a b) = show a ++ " -> " ++ show b

data Operand = Add | Sub | Mul deriving (Eq)

instance Show Operand where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"

-- Expressions
data Expr
  = Unit
  | VBool Bool
  | VInt Int
  | Var Name
  | Lam Name Expr
  | App Expr Expr
  | Op Operand Expr Expr
  | If Expr Expr Expr
  deriving (Eq)

instance Show Expr where
  show Unit = "()"
  show (VBool x) = show x
  show (VInt x) = show x
  show (Var x) = show x
  show (Lam x e) = show x
  show (App e1 e2) = "(" ++ show e1 ++ ") " ++ show e2
  show (Op op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (If v t e) = "if " ++ show v ++ " then " ++ show t ++ " else " ++ show e

type Env = Map String Type

type Constraint = (Type, Type)

-- typechecker

-- Some way to keep track of available fresh ty var names

-- type Infer a = StateT Int (Either String) a
--
-- freshTVar :: Infer Type
-- freshTVar = do
--   n <- get
--   put (n + 1)
--   pure (TVar ("α" ++ show n))
--

tc :: Env -> Expr -> Either String (Type, [Constraint])
-- T-CONST
tc env Unit = pure (B TUnit, [])
tc env (VBool _) = pure (B TBool, [])
tc env (VInt _) = pure (B TInt, [])
-- T-VAR
tc env (Var x) = case lookupVar env x of
  Just ty -> Right (ty, [])
  Nothing -> Left ("Unbound variable: " ++ x)
-- T_LAM
tc env (Lam x body) = do
  let alpha = TVar "α" -- fresh type var for x (placeholder: α) ?? how do I ensure the names don't clash?
  let env' = extend env x alpha
  (bodyTy, bodyCs) <- tc env' body
  pure (TArrow alpha bodyTy, bodyCs)

-- T-APP
tc env (App e1 e2) = do
  (tyE1, cs1) <- tc env e1
  (tyE2, cs2) <- tc env e2
  let beta = TVar "β" -- fresh type var (placeholder: β)
  pure (beta, cs1 ++ cs2 ++ [(tyE1, TArrow tyE2 beta)]) -- tyE1 should be equivalent to (tyE2 -> β)

-- T-ARITH
tc env (Op op e1 e2) = do
  (ty1, cs1) <- tc env e1
  (ty2, cs2) <- tc env e2
  pure (B TInt, cs1 ++ cs2 ++ [(ty1, B TInt), (ty2, B TInt)]) -- both operands should be Int

-- T-IF
tc env (If c t e) = do
  (tyC, csC) <- tc env c
  (tyT, csT) <- tc env t
  (tyE, csE) <- tc env e
  pure (tyT, csC ++ csT ++ csE ++ [(tyC, B TBool), (tyT, tyE)]) -- cond must be Bool, then/else must be equal

type Subst = Map String Type

unifyAll :: [Constraint] -> Either String Subst
unifyAll _ = undefined

applySubst :: Subst -> Type -> Type
applySubst _ _ = undefined

run :: Expr -> Either String Type
run expr = do
  -- (ty, cs) <- evalStateT (tc Map.empty expr) 0
  (ty, cs) <- tc Map.empty expr
  subst <- unifyAll cs
  pure (applySubst subst ty)

extend :: Env -> Name -> Type -> Env
extend env x ty = Data.Map.insert x ty env

lookupVar :: Env -> Name -> Maybe Type
lookupVar env x = Data.Map.lookup x env
