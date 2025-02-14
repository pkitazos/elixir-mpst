module Types where

import Data.List (intercalate)
import Data.Map (Map, insert, lookup)

-------------------------------------------------------------------------------
-- Participants, labels, etc.
-------------------------------------------------------------------------------

type Participant = String

type Label = String

type Name = String

--------------------------------------------------------------------------------
-- Base types
--------------------------------------------------------------------------------

data BaseType
  = TUnit --  "1" in the Maty rules
  | TInt --  "Int"
  | TBool --  "Bool"
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Session types
--------------------------------------------------------------------------------

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
  deriving (Show, Eq)

data OutputST = OutST Participant [(Label, Type, ST)]
  deriving (Show, Eq)

data InputST = InST Participant [(Label, Type, ST)]
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Full types (A, B) in Maty
--------------------------------------------------------------------------------

--
-- A type can be:
--   C                 (base)
--   A ⟶ B             (but with session pre/post: A^(S→T) → B)
--   AP((p_i:S_i)_i)      (access point type)
--   Handler(S?)       (handler type, where S? is an input session type)
--
-- The function type is annotated with pre/post session types:
--    A ^(S,T) → B
--
data Type
  = Base BaseType --  A ^(S,T) → B
  | Func Type ST ST Type
  | APType [(Participant, ST)]
  | HandlerT InputST --  Must be an 'In' session type (p & {...}),
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Values (V)
--------------------------------------------------------------------------------

data Val
  = VVar Name
  | VUnit
  | VBool Bool
  | VInt Int
  | VLam -- \(\lambda x.M\)  with pre= S, post = T for the function’s session constraints
      { lamParam :: Name,
        lamParamType :: Type, -- type of the parameter "x"
        lamPre :: ST, -- function’s precondition
        lamPost :: ST, -- function’s postcondition
        lamBody :: Expr -- body M
      }
  | -- handler p { l_i(x_i) -> M_i }
    VHandler
      { handlerParticipant :: Participant,
        handlerBranches :: [(Label, Name, Type, ST, Expr)] -- Each branch: label l_i, bound var x_i, type of x_i, session precondition for M_i, body M_i
      }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Expressions / Computations (M)
--------------------------------------------------------------------------------

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
  deriving (Show, Eq)

type Env = Map String Type
