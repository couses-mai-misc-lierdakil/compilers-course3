module Main where

import Lexer

main :: IO ()
main = interact (unlines . map (show . lexer) . lines)
