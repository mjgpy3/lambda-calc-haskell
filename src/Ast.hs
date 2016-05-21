module Ast where

data Ast =
  Name Char
  | Application Ast Ast
  | Lambda Char Ast
  | Closure Char Ast [(Char, Ast)]
