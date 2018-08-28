module Tokeniser where

import Spec
import Data.Char


split :: String -> [String]
split []       = []
split ('+':xs) = ["+"] ++ (split xs)
split ('*':xs) = ["*"] ++ (split xs)
split ('/':xs) = ["/"] ++ (split xs)
split ('-':xs) = ["-"] ++ (split xs)
split ('=':xs) = ["="] ++ (split xs)
split (',':xs) = [","] ++ split xs
split (' ':xs) = split xs
split ('(':xs) = ["("] ++ split xs
split (')':xs) = [")"] ++ split xs
split xs
    | isDigit $ head xs = [(takeWhile isDigit xs)] ++ (split (dropWhile isDigit xs))
    | head xs == '$' = [(takeWhile isParam xs)] ++ (split (dropWhile isParam xs))
    | otherwise = [(takeWhile isAlpha xs)] ++ (split (dropWhile isAlpha xs))
        where isParam c = isDigit c || c == '$'


checkTokens :: String -> Bool
checkTokens [] = True
checkTokens (x:xs) = if elem x valid then checkTokens xs else False
    where valid = " 0123456789abcdefghijklmnopqrstuvwxyz+=*(),-/$"
