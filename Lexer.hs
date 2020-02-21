{-# OPTIONS_GHC -Wall -Wextra #-}
module Main where

import Data.Char (isAsciiLower, isDigit)

data TokenName =
    Number
  | Operator
  | Id
  | Lparen
  | Rparen
  | Comma
  deriving (Show)

type Token = (TokenName, String)
type State = Int

stateToToken :: String -> State -> Maybe Token
stateToToken inp state =
  case state of
    1  -> Just (Number  , inp)
    2  -> Just (Number  , inp)
    5  -> Just (Number  , inp)
    6  -> Just (Operator, inp)
    7  -> Just (Id      , inp)
    8  -> Just (Lparen  , inp)
    9  -> Just (Rparen  , inp)
    10 -> Just (Comma   , inp)
    _  -> Nothing

transitionTable :: State -> Char -> Maybe State
transitionTable state char =
  case state of
    0 | isLetter char       -> Just 7
      | char == '('         -> Just 8
      | char == ')'         -> Just 9
      | char == ','         -> Just 10
      | char `elem` "+*/^%-"-> Just 6
      | isDigit char        -> Just 1
    1 | isDigit char        -> Just 1
      | char == '.'         -> Just 2
    2 | isDigit char        -> Just 2
      | char `elem` "eE"    -> Just 3
    3 | char `elem` "+-"    -> Just 4
      | isDigit char        -> Just 5
    4 | isDigit char        -> Just 5
    5 | isDigit char        -> Just 5
    7 | isDigit char || isLetter char
                            -> Just 7
    _                       -> Nothing
  where
  isLetter c = isAsciiLower c || c == '_'

data DFAAction = Continue | Restart

type StateWIO = (String, String, State)
              -- input , output, st
type DFAState = (StateWIO, Maybe StateWIO)
              -- (input, st)    , last accepting state

dfaStep :: DFAState -> (DFAAction, DFAState)
dfaStep ds =
  let ((input, output, st), lac) = ds
      nextState = transitionTable st (head input)
      newAcceptingState =
        maybe lac (const $ Just (input, output, st))
        $ stateToToken output st
      input' = tail input
      output' = output <> [head input]
  in if null input
     then (Restart, (undefined, newAcceptingState))
     else  maybe (Restart, (undefined, newAcceptingState))
                 (\x -> (Continue, ((input', output', x), newAcceptingState)))
                 nextState

dfaLoop :: DFAState -> [Maybe Token]
dfaLoop st =
  case dfaStep st of
    (Continue, st') -> dfaLoop st'
    (Restart, (_, Just accSt))
        -> stateToToken output st' : dfaLoop ((input, "", 0), Nothing)
        where (input, output, st') = accSt
    (Restart, (_, Nothing)) -> []

mainLoop :: String -> String
mainLoop input = show $ dfaLoop ((input, "", 0), Nothing)

main :: IO ()
main = interact mainLoop
