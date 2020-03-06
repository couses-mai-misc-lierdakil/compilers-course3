module Main where

import Lexer
import RecursiveParser
import SimpleInterpreter

main :: IO ()
main = interact (unlines . map (show . run) . lines)
  where run xs = interpret [] =<< (parse . lexer $ xs)
