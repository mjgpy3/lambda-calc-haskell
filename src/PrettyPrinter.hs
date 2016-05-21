module PrettyPrinter (pretty) where

import Ast

pretty :: Ast -> String
pretty (Name name) = [name]
pretty (Application fn arg) = concat ["(", pretty fn, " ", pretty arg, ")"]
pretty (Lambda arg body) = concat ["\\", [arg], ".", pretty body]
pretty (Closure arg body _) = concat ["\\", [arg], ".", pretty body]
