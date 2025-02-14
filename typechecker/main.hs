module Main where

import Data.Map qualified as Map
import TypeChecker
import Types

main :: IO ()
main = do
  putStrLn "Running some typechecker tests..."

  -- 1. EApp (λx:TBool.Pre=End->Post=End) True
  --    We'll check it under an empty environment and session End.
  let test1 = tcExpr Map.empty End exApp
  putStrLn "=== Apply inline lambda to True ==="
  case test1 of
    Left err -> do
      putStrLn $ "Type error: " ++ err
    Right (ty, st) -> do
      putStrLn $ "Computed type: " ++ show ty
      putStrLn $ "Remaining session type: " ++ show st
  putStrLn ""

  -- 2. spawn (return ())
  --    The current session can be anything, e.g. End, so let's pass End.
  let test2 = tcExpr Map.empty End exSpawn
  putStrLn "=== Spawn 'return ()' under End ==="
  case test2 of
    Left err -> do
      putStrLn $ "Type error: " ++ err
    Right (ty, st) -> do
      putStrLn $ "Computed type: " ++ show ty
      putStrLn $ "Remaining session type: " ++ show st
  putStrLn ""

  -- 3. Return the handler as a value
  --    We'll type-check the value (not an expression),
  --    so we use tcVal with an empty environment.
  let test3 = tcVal Map.empty simpleHandler
  putStrLn "=== Handler test ==="
  case test3 of
    Left err -> putStrLn $ "Type error: " ++ err
    Right t -> putStrLn $ "Handler type: " ++ show t
  putStrLn ""

  putStrLn "Done!"

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
