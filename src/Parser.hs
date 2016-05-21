module Parser (parse) where

import qualified Ast as A
import qualified Token as T

parse :: [T.Token] -> A.Ast
parse = fst . parseSingle

parseSingle :: [T.Token] -> (A.Ast, [T.Token])
parseSingle (T.Name name:rest) = (A.Name name, rest)
parseSingle (T.Lambda:T.Name arg:T.Dot:rest) =
  overFirst (A.Lambda arg) $ parseSingle rest
parseSingle (T.LParen:rest) =
  let
    (func, rest1) = parseSingle rest
    (arg, _:rest2) = parseSingle rest1
  in
    (A.Application func arg, rest2)

overFirst :: (a -> b) -> (a, c) -> (b, c)
overFirst fn (x, y) = (fn x, y)
