module Types where

import Data.List (intercalate)
import Data.Map (Map, insert, lookup)

-- Aliases
type Participant = String

type Label = String

type Name = String

-- Base types
data BaseType
  = TUnit
  | TInt
  | TBool
  | TString
  deriving (Eq)

instance Show BaseType where
  show TUnit = "1"
  show TInt = "Int"
  show TBool = "Bool"

-- Session types
--
-- S (session types) can be:
--   end
--   p ⊕ {l_i(A_i).S_i}_i  (Out or Send)
--   p & {l_i(A_i).S_i}_i   (In or Recv)
--

data ST
  = End
  | SOut OutputST
  | SIn InputST
  | SAnnotation String
  deriving (Eq)

instance Show ST where
  show End = "end"
  show (SOut s) = show s
  show (SIn s) = show s

data OutputST = OutST Participant [(Label, Type, ST)]
  deriving (Eq)

data InputST = InST Participant [(Label, Type, ST)]
  deriving (Eq)

showBranch :: (Label, Type, ST) -> String
showBranch (label, val, s) = label ++ "(" ++ show val ++ ")" ++ "." ++ show s

instance Show OutputST where
  show (OutST p msgs) = p ++ "+{ " ++ intercalate ", " (map showBranch msgs) ++ " }"

instance Show InputST where
  show (InST p msgs) = p ++ "&{ " ++ intercalate ", " (map showBranch msgs) ++ " }"

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
  | Func Type ST (Maybe ST) (Maybe Type) --  A -(S,T)→ B
  | APType [(Participant, ST)]
  | HandlerT InputST --  Must be an 'In' session type (p & {...}),
  deriving (Eq)

instance Show Type where
  show (Base b) = show b
  show (Func a s t (Just b)) = show a ++ " -(" ++ show s ++ ", " ++ show t ++ ")-> " ++ show b
  show (Func a s t Nothing) = show a ++ " -(" ++ show s ++ ", " ++ show t ++ ")-> "
  show (APType ps) = "AP(" ++ intercalate "; " (map (\(p, st) -> p ++ ": " ++ show st) ps) ++ ")"
  show (HandlerT s) = "Handler(" ++ show s ++ ")"

-- Values (V)
data Val
  = VVar Name
  | VUnit
  | VBool Bool
  | VStr String
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
        handlerBranches :: [(Label, Name, Type, ST, Expr)] -- Each branch: label l_i, bound var x_i, type of x_i, session precondition for M_i, body M_i
      }
  deriving (Eq)

showHandlerBranch :: (Label, Name, Type, ST, Expr) -> String -- ! not super happy with this either
showHandlerBranch (label, x, t, s, e) = label ++ "(" ++ x ++ " : " ++ show t ++ ") . " ++ show s ++ " => " ++ show e

instance Show Val where
  show VUnit = "()"
  show (VVar x) = show x
  show (VInt x) = show x
  show (VBool x) = show x
  show (VLam paramName paramTy pre post body) = "λ " ++ paramName ++ " : " ++ show paramTy ++ " -(" ++ show pre ++ ", " ++ show post ++ ")->" ++ ". " ++ show body
  show (VHandler p bs) = "handler " ++ p ++ "&{ " ++ intercalate "; " (map showHandlerBranch bs) ++ " }"

-- Expressions / Computations (M)
data Expr
  = EReturn Val
  | ECont Expr Expr
  | ELet Name Expr Expr -- let x <== M in N
  | EApp Val Val -- V W
  | EIf Val Expr Expr -- if V then M else N
  | ESpawn Expr -- spawn M
  | ESend Participant Label Val -- p ! l(V)
  | ESuspend Val -- suspend V
  | ENewAP [(Participant, ST)] -- newAP((p_i : S_i)_i)
  | ERegister Val Participant Expr
  deriving (Eq)

instance Show Expr where
  show (EReturn v) = "return " ++ show v
  show (ECont e1 e2) = show e1 ++ " ; " ++ show e2
  show (ELet x m n) = "let " ++ x ++ " <== " ++ show m ++ " in " ++ show n
  show (EApp v1 v2) = "(" ++ show v1 ++ ") " ++ show v2
  show (EIf v t e) = "if " ++ show v ++ " then " ++ show t ++ " else " ++ show e
  show (ESpawn m) = "spawn " ++ show m
  show (ESend p l v) = p ++ " ! " ++ l ++ "(" ++ show v ++ ")"
  show (ESuspend v) = "suspend " ++ show v
  show (ENewAP ps) = "newAP(" ++ intercalate "; " (map (\(p, st) -> p ++ ": " ++ show st) ps) ++ ")"
  show (ERegister v p m) = "register " ++ show v ++ " " ++ p ++ " " ++ show m

type Env = Map String Type

type ST_Env = Map String ST
