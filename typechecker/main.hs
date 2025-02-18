module Main where

import Data.Map qualified as Map
import TypeChecker
import Types

main :: IO ()
main = do
  putStrLn "Running a suite of typechecker tests...\n"

  ----------------------------------------------------------------
  -- 1. Test Value: a basic integer
  ----------------------------------------------------------------
  testVal
    "Basic integer constant"
    emptyEnv
    (VInt 123)
  -- Expect: Type = Int

  ----------------------------------------------------------------
  -- 2. Test Value: a lambda that does NOT suspend
  ----------------------------------------------------------------
  let lamNoSuspend = VLam "x" (Base TBool) End End (EReturn (VVar "x"))
  testVal
    "Lambda that returns x (Bool) with pre=End, post=End"
    emptyEnv
    lamNoSuspend
  -- Expect: Success, type = (Bool)-(end,end)->Bool

  ----------------------------------------------------------------
  -- 3. Test Value: a lambda that DOES suspend
  --    We simulate 'suspend' in the body, so the function might produce Nothing.
  ----------------------------------------------------------------
  let lamSuspend =
        VLam
          "x"
          (Base TBool)
          End
          End
          (ESuspend (VHandler "p" [])) -- forcibly suspends with empty handler
  testVal
    "Lambda that suspends in the body"
    emptyEnv
    lamSuspend
  -- Expect: Success, type = (Bool)-(end,end)-> (Nothing)

  -- Type error: Invalid expression (or partial rule coverage): suspend handler p {  }

  ----------------------------------------------------------------
  -- 4. Test Expr: let-binding with normal subexpr
  ----------------------------------------------------------------
  let normalLet =
        ELet
          "y"
          (EReturn (VInt 999)) -- subexpr returns (Int, End)
          (EReturn (VVar "y")) -- use 'y' in the next expression
  testExpr
    "Let-binding normal subexpr"
    emptyEnv
    End
    normalLet
  -- Expect: subexpr = (Int, End), so the final type is (Int, End)

  ----------------------------------------------------------------
  -- 5. Test Expr: let-binding that suspends
  --    subexpr = ESuspend => entire let also suspends => yields Nothing
  ----------------------------------------------------------------
  let suspLet =
        ELet
          "y"
          (ESuspend (VHandler "q" []))
          (EReturn (VVar "y")) -- We never get here
  testExpr
    "Let-binding that suspends in subexpr"
    emptyEnv
    End
    suspLet
  -- Expect: subexpr => Nothing => entire let => Nothing

  -- ! Type error: Invalid expression (or partial rule coverage): suspend handler p {  }

  ----------------------------------------------------------------
  -- 6. Test Expr: spawn normal
  --    spawn (return ()) under End => subexpr returns (Unit, End)
  ----------------------------------------------------------------
  let spawnNormal = ESpawn (EReturn VUnit)
  testExpr
    "Spawn normal, subexpr returns unit"
    emptyEnv
    End
    spawnNormal
  -- Expect: subexpr is (Unit, End), so the spawn result => (Unit, End).
  -- Actually you pick (Unit, original pre)...

  ----------------------------------------------------------------
  -- 7. Test Expr: spawn that suspends
  --    spawn (ESuspend someHandler)
  ----------------------------------------------------------------
  let spawnSusp =
        ESpawn
          ( ESuspend
              (VHandler "p" [])
          )
  testExpr
    "Spawn that suspends"
    emptyEnv
    End
    spawnSusp
  -- Expect: subexpr => Nothing => we do Just (Unit, End) if you adopt that logic.

  -- ! Type error: Invalid expression (or partial rule coverage): suspend handler p {  }

  ----------------------------------------------------------------
  -- 8. Test Expr: if with two returning branches
  ----------------------------------------------------------------
  let ifRet =
        EIf
          (VBool True)
          (EReturn (VInt 1))
          (EReturn (VInt 2))
  testExpr
    "If both branches return Int"
    emptyEnv
    End
    ifRet
  -- Expect: merges to (Int, End)

  ----------------------------------------------------------------
  -- 9. Test Expr: if that merges suspend & returning branch
  ----------------------------------------------------------------
  let ifMixed =
        EIf
          (VBool True)
          (ESuspend (VHandler "p" [])) -- branch suspends => Nothing
          (EReturn (VInt 42)) -- branch returns (Int, End)
  testExpr
    "If merges a suspending branch with returning branch"
    emptyEnv
    End
    ifMixed
  -- If your code merges "Nothing" and "Just (Int, End)" => "Just (Int, End)" or similar
  -- depending on your 'mergeSuspends' logic.

  -- ! Type error: Invalid expression (or partial rule coverage): suspend handler p {  }

  ----------------------------------------------------------------
  -- 10. Test Expr: ESend
  --    We'll create a session type SOut (OutST "p" [...]) that expects label "foo"
  ----------------------------------------------------------------
  let outST =
        SOut
          ( OutST
              "p"
              [("foo", Base TInt, End)] -- label=foo, payload=Int => End
          )
      sendFoo =
        ESend "p" "foo" (VInt 10)
  testExpr
    "Send a label foo with int 10, correct payload & label"
    emptyEnv
    outST
    sendFoo
  -- Expect: (Unit, End)

  putStrLn "Done!\n"

----------------------------------------------------------------

-- Test a computation (Expr)
testExpr ::
  String -> -- Test name / description
  Env -> -- Type environment
  ST -> -- Current session precondition
  Expr -> -- Expression to typecheck
  IO ()
testExpr description env pre expr = do
  putStrLn $ "=== " ++ description ++ " ==="
  putStrLn $ "Expression: " ++ show expr
  case tcExpr env pre expr of
    Left err -> do
      putStrLn $ "Type error: " ++ err
    Right Nothing -> do
      putStrLn $ "Result: suspend (Nothing)."
    Right (Just (ty, st')) -> do
      putStrLn $ "Success! Computed type: " ++ show ty
      putStrLn $ "Remaining session type: " ++ show st'
  putStrLn ""

-- Test a value (Val)
testVal ::
  String -> -- Test name / description
  Env -> -- Type environment
  Val -> -- Value to typecheck
  IO ()
testVal description env val = do
  putStrLn $ "=== " ++ description ++ " ==="
  putStrLn $ "Value: " ++ show val
  case tcVal env val of
    Left err -> do
      putStrLn $ "Type error: " ++ err
    Right ty -> do
      putStrLn $ "Success! Value has type: " ++ show ty
  putStrLn ""

-- shorthand utils

emptyEnv :: Env
emptyEnv = Map.empty

boolVal :: Val
boolVal = VBool True

intVal :: Val
intVal = VInt 42

unitVal :: Val
unitVal = VUnit

endST :: ST
endST = End
