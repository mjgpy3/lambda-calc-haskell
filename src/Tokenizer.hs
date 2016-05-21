module Tokenizer (tokenize) where

import Token

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

tokenize :: String -> [Token]
tokenize ('(':rest) = LParen:tokenize rest
tokenize (')':rest) = RParen:tokenize rest
tokenize ('\\':rest) = Lambda:tokenize rest
tokenize ('.':rest) = Dot:tokenize rest
tokenize (c:rest) = (if c `elem` alphabet then [Name c] else []) ++ tokenize rest
