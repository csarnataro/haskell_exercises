module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = whatchamacallit (not . p) xs 

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = whatchamacallit p xs

whatchamacallit :: (a -> Bool) -> [a] -> [a]
whatchamacallit _ [] = []
whatchamacallit p (x:xs) =  if (p x) 
                            then (x:whatchamacallit p xs) 
                            else whatchamacallit p xs
