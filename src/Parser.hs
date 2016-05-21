module Parser (parse) where

import qualified Ast as A
import qualified Token as T

parse :: [T.Token] -> A.Ast
parse = fst . parseSingle

parseSingle :: [T.Token] -> (A.Ast, [T.Token])
parseSingle (T.Name name:rest) = (A.Name name, rest)
parseSingle (T.Lambda:T.Name arg:T.Dot:rest) =
  let
    (body, rest2) = parseSingle rest
  in
    (A.Lambda arg body, rest2)
parseSingle (T.LParen:rest) =
  let
    (func, rest1) = parseSingle rest
    (arg, _:rest2) = parseSingle rest1
  in
    (A.Application func arg, rest2)
