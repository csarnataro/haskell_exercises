module Isogram (isIsogram) where

import Data.List (sort, group)
import Data.Char

isIsogram :: String -> Bool
isIsogram x = isSanitizedIsogram $ filter (\c -> c /= ' ' && c /= '-' ) x

isSanitizedIsogram :: String -> Bool
isSanitizedIsogram x
  | length x <= 1 = True
  | length x > 26 = False
  | otherwise =
      (==) 0 
      $ length
      $ filter (\g -> length g > 1)
      $ group 
      $ sort 
      $ map toLower x
