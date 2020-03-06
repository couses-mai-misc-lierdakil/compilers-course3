{-# OPTIONS_GHC -Wall -Wextra #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Maybe
import Data.Char

data TokenName =
    Number
  | Operator
  | Id
  | Lparen
  | Rparen
  | Comma
  deriving Show

data Token = Token { tokenName :: TokenName, tokenStr :: String } deriving Show
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
  isLetter ch = isAsciiLower ch || ch == '_'

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

data FullState = FullState { fsInput :: String
                           , fsOutput :: String
                           , fsCurrentState :: StateNum
                           }
data Baggage = Baggage { bagCurrentState :: FullState
                       , bagLastAccState :: Maybe FullState
                       }
              -- current st., last seen accepting st.
data Action = Continue | Restart | Stop

dfaStep :: Baggage -> (Action, Baggage)
dfaStep b =
  let Baggage{..} = b
      FullState{..} = bagCurrentState
      nextState = transitionTable fsCurrentState (head fsInput)
      newAccSt =
        maybe bagLastAccState
              (const $ Just bagCurrentState)
              $ stateToTokenName fsCurrentState
      input' = tail fsInput
      output' = if head fsInput == ' '
                then fsOutput
                else fsOutput <> [head fsInput]
  in if null fsInput
     then (Stop, Baggage{bagCurrentState = bagCurrentState
                        ,bagLastAccState = newAccSt })
     else maybe (Restart, Baggage{bagCurrentState = bagCurrentState
                                 ,bagLastAccState = newAccSt })
                (\st -> (Continue,
                        Baggage{bagCurrentState = FullState {
                                  fsInput = input'
                                , fsOutput = output'
                                , fsCurrentState = st
                                }
                               , bagLastAccState = newAccSt }
                ))
                nextState

dfaRun :: Baggage -> [Token]
dfaRun b =
  case dfaStep b of
    (Stop, Baggage _ (Just (FullState _ output' lastAccStNum)))
      -> [Token (fromJust $ stateToTokenName lastAccStNum) output']
    (Stop, Baggage _ Nothing)
      -> []
    (Restart, Baggage _ (Just (FullState input' output' lastAccStNum)))
      -> Token (fromJust $ stateToTokenName lastAccStNum) output' : dfaRun (initState input')
    (Restart, Baggage _ Nothing)
      -> []
    (Continue, bag)
      -> dfaRun bag

initState :: String -> Baggage
initState input = Baggage (FullState input "" 0) Nothing

lexer :: String -> String
lexer input = show $ dfaRun (initState input)

main :: IO ()
main = interact (unlines . map lexer . lines)
