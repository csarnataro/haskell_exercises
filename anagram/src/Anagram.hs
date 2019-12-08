module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor _ [] = []
anagramsFor xs xss = filter (matchStrings xs) xss

matchStrings :: String -> String -> Bool
matchStrings x xs = 
  (lc x /= lc xs) 
  && (sort $ lc x) == (sort $ lc xs)

lc :: String -> String
lc s = map toLower s