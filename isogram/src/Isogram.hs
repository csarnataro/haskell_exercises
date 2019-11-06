module Isogram (isIsogram) where

import Data.List (sort, group)

import Data.Char


isIsogram :: String -> Bool
isIsogram [] = True
isIsogram x = do
  let sanitized = filter (\c -> c /= ' ' && c /= '-' ) x
  if (length sanitized) <= 1 || (length sanitized) > 26 
  then
    False 
  else
    (==) 0 
    $ length
    $ filter (\g -> (length g) > 1)
    $ group 
    $ sort 
    $ filter (\c -> c /= ' ' && c /= '-' ) 
    $ map toLower x
