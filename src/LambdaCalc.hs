module LambdaCalc where

import System.Environment

import Evaluator
import Parser
import PrettyPrinter
import Tokenizer

interp :: String -> String
interp = pretty . eval . parse . tokenize

main :: IO ()
main = fmap (interp . (!! 0)) getArgs >>= putStrLn
