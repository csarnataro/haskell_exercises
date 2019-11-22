module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate s = findAcronym $ zipStrings s

zipStrings :: String -> [(Char, Char)]
zipStrings s = zip (" " ++ s) (s ++ " ")

checkIfInitial :: (Char, Char) -> Char
checkIfInitial pair
  | ((fst pair) == ' ') && (isAlpha (snd pair)) = toUpper $ snd pair
  | ((fst pair) == '-') && (isAlpha (snd pair)) = toUpper $ snd pair
  | ((fst pair) == '_') && (isAlpha (snd pair)) = toUpper $ snd pair
  | (isLower (fst pair)) && (isUpper (snd pair)) = snd pair
  | otherwise = ' '

findAcronym :: [(Char, Char)] -> [Char]
findAcronym pairs = filter (\x -> x /= ' ' ) (map checkIfInitial pairs)
