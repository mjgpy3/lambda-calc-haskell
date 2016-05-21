module Evaluator (eval) where

import Ast
import Data.Maybe (fromJust)

eval :: Ast -> Ast
eval = evalEnv []

evalEnv :: [(Char, Ast)] -> Ast -> Ast
evalEnv env (Lambda arg body) = Closure arg body env
evalEnv env (Name name) = fromJust $ lookup name env
evalEnv env (Application fn arg) =
  let
    closure@(Closure argName body closedEnv) = evalEnv env fn
    value = evalEnv env arg
  in
    evalEnv ((argName, value):closedEnv ++ env) body
