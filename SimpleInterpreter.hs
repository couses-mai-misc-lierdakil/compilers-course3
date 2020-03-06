module SimpleInterpreter where

import RecursiveParser
import Control.Applicative

opFunc :: Operator -> Double -> Double -> Double
opFunc OpPlus = (+)
opFunc OpMinus = (-)
opFunc OpMult = (*)
opFunc OpDiv = (/)
opFunc OpExp = (**)

interpret :: [(String, Double)] -> Expr -> Either String Double
interpret vars = go
  where
  go (BinOp op e1 e2) = liftA2 (opFunc op) (go e1) (go e2)
  go (Identifier str) =
    maybe (Left $ "Undefined variable " <> str) Right $ lookup str vars
  go (ENumber val) = Right val
  go (UnaryMinus e1) = fmap negate (go e1)
