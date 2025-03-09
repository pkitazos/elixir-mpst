module Main (main) where

import Data.Map (Map, empty, insert, lookup)
import TypeChecker
import Types

main :: IO ()
main = do
  putStrLn "hello world"

vars :: Env
vars = empty

sessions :: [(ST, ST)] -> ST_Env
sessions (((SAnnotation a), st) : ss) = insert a st empty

registerAnnotation :: (ST, ST) -> ST_Env -> ST_Env
registerAnnotation ((SAnnotation a), st) env = insert a st env

seller :: Val -> Val
seller ap =
  VLam
    { lamParam = "ap",
      lamParamType = (APType [("Buyer1", End), ("Buyer2", End), ("Seller", End)]),
      lamPre = End,
      lamPost = End,
      lamBody = (ERegister ap "Seller" (ESuspend title_handler))
    }

buyer1 :: (Val, Val) -> Val
buyer1 (ap, title) =
  VLam
    { lamParam = "ap",
      lamParamType = (APType [("Buyer1", End), ("Buyer2", End), ("Seller", End)]),
      lamPre = End,
      lamPost = End,
      lamBody = (ERegister ap "Buyer1" (ECont (ESend "Seller" "title" title) (ESuspend title_handler)))
    }

buyer2 :: (Val) -> Val
buyer2 (ap) =
  VLam
    { lamParam = "ap",
      lamParamType = (APType [("Buyer1", End), ("Buyer2", End), ("Seller", End)]),
      lamPre = End,
      lamPost = End,
      lamBody = (ERegister ap "Buyer1" (ESuspend share_handler))
    }

--
--

title_handler_ST = SAnnotation "Buyer1 & { title(x) . Buyer1 ! quote(num) . decision_handler }"

title_handler =
  VHandler
    "Buyer1"
    [ ( "title",
        "x",
        (Base TInt),
        title_handler_ST,
        (ECont (ESend "Buyer1" "quote" (VInt 10)) (ESuspend decision_handler))
      )
    ]

--
--
decision_handler_ST = SAnnotation "Buyer2 & { address(addr) . Buyer2 ! quote(num) . end, quit(_) . end }"

decision_handler =
  VHandler
    "Buyer2"
    [ ("address", "addr", (Base TInt), decision_handler_ST, (ESend "Buyer2" "date" (VStr "Mar 14"))),
      ("quit", "_", (Base TBool), decision_handler_ST, EReturn VUnit)
    ]

--
--
quote_handler_ST = SAnnotation "Seller & quote(amount) . Buyer2 ! share(other_amount)"

quote_handler =
  VHandler
    "Seller"
    [ ("quote", "amount", (Base TInt), quote_handler_ST, (ESend "Buyer2" "share" (VInt 100)))
    ]

--
--
share_handler_ST = SAnnotation "Buyer1 & share(amount) . Seller ! { address(addr) . date_handler, quit(_) . end}"

share_handler =
  VHandler
    "Buyer1"
    [ ("share", "amount", (Base TInt), share_handler_ST, (EIf ((VBool True)) (ESend "Seller" "quit" (VUnit)) (ESuspend date_handler)))
    ]

--
--
date_handler_ST = SAnnotation "Seller & date(date) . end"

date_handler =
  VHandler
    "Seller"
    [ ("date", "date", (Base TString), date_handler_ST, (EReturn VUnit))
    ]
