{-# OPTIONS_GHC -Wall -Wextra #-}
module RecursiveParser where

import Lexer

data Expr =
    BinOp Operator Expr Expr
  | Identifier String
  | ENumber Double
  | UnaryMinus Expr
  deriving Show

data Operator =
    OpPlus
  | OpMinus
  | OpMult
  | OpDiv
  | OpExp
  deriving Show

parse :: [Token] -> Either String Expr
parse ts = case parseE ts of
    Right (rest, eexpr) ->
      if null rest
      then Right eexpr
      else reportError "end of input" rest
    Left err -> Left err

parseE :: [Token] -> Either String ([Token], Expr)
parseE ts = either Left (uncurry parseE') $ parseT ts

parseT :: [Token] -> Either String ([Token], Expr)
parseT ts = either Left (uncurry parseT') $ parseF ts

parseE' :: [Token] -> Expr -> Either String ([Token], Expr)
parseE' ts t1 =
  case ts of
    Token Operator "+" : ts'
      -> case parseT ts' of
            Right (rest, texpr) -> Right (rest, BinOp OpPlus t1 texpr)
            Left err -> Left err
    Token Operator "-" : ts'
      -> case parseT ts' of
            Right (rest, texpr) -> Right (rest, BinOp OpMinus t1 texpr)
            Left err -> Left err
    _ -> Right (ts, t1)

parseT' :: [Token] -> Expr -> Either String ([Token], Expr)
parseT' ts t1 =
  case ts of
    Token Operator "*" : ts'
      -> case parseT ts' of
            Right (rest, texpr) -> Right (rest, BinOp OpMult t1 texpr)
            Left err -> Left err
    Token Operator "/" : ts'
      -> case parseT ts' of
            Right (rest, texpr) -> Right (rest, BinOp OpDiv t1 texpr)
            Left err -> Left err
    _ -> Right (ts, t1)

parseF :: [Token] -> Either String ([Token], Expr)
parseF ts = either Left (uncurry parseF') $ parseV ts

parseF' :: [Token] -> Expr -> Either String ([Token], Expr)
parseF' ts t1 =
  case ts of
    Token Operator "^" : ts'
      -> case parseF ts' of
          Right (ts'', fexpr) -> Right (ts'', BinOp OpExp t1 fexpr)
          Left err -> Left err
    _ -> Right (ts, t1)

parseV :: [Token] -> Either String ([Token], Expr)
parseV ts =
  case ts of
    Token Lparen _ : ts'
      -> case parseE ts' of
            Right (rest, eexpr) ->
                case rest of
                  Token Rparen _ : rest' -> Right (rest', eexpr)
                  x -> reportError ")" x
            Left err -> Left err
    Token Id identifierStr : ts'
      -> Right (ts', Identifier identifierStr)
    Token Number numberStr : ts'
      -> Right (ts', ENumber $ read numberStr)
    Token Operator "-" : ts'
      -> case parseV ts' of
            Right (rest, fexpr) -> Right (rest, UnaryMinus fexpr)
            Left err -> Left err
    x -> reportError "(, id, num or -" x

reportError :: String -> [Token] -> Either String a
reportError msg ts = Left $ "Expected " <> msg <> " but got " <> case ts of
  Token _ str : _ -> str
  [] -> "end of input"
