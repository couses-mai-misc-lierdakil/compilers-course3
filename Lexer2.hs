{-# OPTIONS_GHC -Wall -Wextra #-}
module Main where

import Data.Maybe

data TokenName =
    Number
  | Operator
  | Id
  | Lparen
  | Rparen
  | Comma
  deriving Show

type Token = (TokenName, String)
type StateNum = Int

transitionTable :: StateNum -> Char -> Maybe StateNum
transitionTable st c =
  case st of
    0 | isLetter c              -> Just 7
      | c == '('                -> Just 8
      | c == ')'                -> Just 9
      | c == ','                -> Just 10
      | c `elem` "+*/^%-"       -> Just 6
      | isDigit c               -> Just 1
      | c == ' '                -> Just 0
    1 | isDigit c               -> Just 1
      | c == '.'                -> Just 2
    2 | isDigit c               -> Just 2
      | c `elem` "eE"           -> Just 3
    3 | c `elem` "+-"           -> Just 4
      | isDigit c               -> Just 5
    4 | isDigit c               -> Just 5
    5 | isDigit c               -> Just 5
    7 | isLetter c || isDigit c -> Just 7
    _                           -> Nothing
  where
  isLetter ch = (ch >= 'a' && ch <= 'z') || ch == '_'
  isDigit ch = ch >= '0' && ch <= '9'

stateToTokenName :: StateNum -> Maybe TokenName
stateToTokenName st =
  case st of
    1  -> Just Number
    2  -> Just Number
    5  -> Just Number
    6  -> Just Operator
    7  -> Just Id
    8  -> Just Lparen
    9  -> Just Rparen
    10 -> Just Comma
    _  -> Nothing

type FullState = (String, String, StateNum)
                -- input, output, current st.
type Baggage = (FullState, Maybe FullState)
              -- current st., last seen accepting st.
data Action = Continue | Restart | Stop

dfaStep :: Baggage -> (Action, Baggage)
dfaStep b =
  let (curFullSt, lastAccSt) = b
      (input, output, curState) = curFullSt
      nextState = transitionTable curState (head input)
      newAccSt =
        maybe lastAccSt
              (const $ Just curFullSt)
              $ stateToTokenName curState
      input' = tail input
      output' = if head input == ' '
                then output
                else output <> [head input]
  in if null input
     then (Stop, (curFullSt, newAccSt))
     else maybe (Restart, (curFullSt, newAccSt))
                (\st -> (Continue, ((input', output', st), newAccSt)))
                nextState

dfaRun :: Baggage -> [Token]
dfaRun b =
  case dfaStep b of
    (Stop, (_, Just (_, output', lastAccStNum)))
      -> [(fromJust $ stateToTokenName lastAccStNum, output')]
    (Stop, (_, Nothing))
      -> []
    (Restart, (_, Just (input', output', lastAccStNum)))
      -> (fromJust $ stateToTokenName lastAccStNum, output') : dfaRun (initState input')
    (Restart, (_, Nothing))
      -> []
    (Continue, bag)
      -> dfaRun bag

initState :: String -> Baggage
initState input = ((input, "", 0), Nothing)

lexer :: String -> String
lexer input = show $ dfaRun (initState input)

main :: IO ()
main = interact (unlines . map lexer . lines)
