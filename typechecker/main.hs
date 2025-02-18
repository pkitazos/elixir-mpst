module Main where

import Data.Map qualified as Map
import TypeChecker
import Types

main :: IO ()
main = do
  putStrLn "add tests here"

----------------------------------------------------------------

-- Example 1: (λx. return x) applied to True
--   The lambda has param x : Bool, with pre=End and post=End, returning 'x'
exLambda :: Val
exLambda =
  VLam
    { lamParam = "x",
      lamParamType = Base TBool,
      lamPre = End,
      lamPost = End,
      lamBody = EReturn (VVar "x")
    }

exApp :: Expr
exApp = EApp exLambda (VBool True)

-- Example 2: spawn (return ()) under any session
--   as per the T-Spawn rule, the spawned expression itself must be typed with
--   pre=End -> post=End, returning TUnit.
exSpawned :: Expr
exSpawned = EReturn VUnit -- typed as TUnit, post=End if we check with st=End

exSpawn :: Expr
exSpawn = ESpawn exSpawned

-- Example 3: a single-branch handler that expects a label "foo".
--   The body of the branch just returns unit, with pre=End -> post=End.
simpleHandler :: Val
simpleHandler =
  VHandler
    { handlerParticipant = "p",
      handlerBranches =
        [ ( "foo", -- label
            "x", -- bound variable
            Base TInt, -- type of x
            End, -- pre-ST for branch
            EReturn VUnit -- body returns unit
          )
        ]
    }
