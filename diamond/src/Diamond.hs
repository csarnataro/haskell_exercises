module Diamond (diamond) where

import Data.Char

diamond :: Char -> Maybe [String]
diamond 'A' = Just ["A"]
diamond d = 
  if (d `elem` ['A'..'Z'])
  then Just 
    $ map 
      (\x -> line x (length (enumFromTo 'A' d))) 
      $ listChars d
       -- from A to <char> ... and back, without repeating <char>
  else 
    Nothing 

line :: Char -> Int -> String
line 'A' len = (outside 'A' len) ++ ['A'] ++ (outside 'A' len)
line b len = (outside b len) ++ [b] ++ (inside b) ++ [b] ++ (outside b len)

minus :: Char -> Char -> Int
minus a b = (ord a) - (ord b)

minusA :: Char -> Int
minusA c = c `minus` 'A'

inside :: Char -> String
inside a = replicate (2 * (minusA a) - 1) ' '

outside :: Char -> Int -> String
outside 'A' len = replicate (len - 1) ' '
outside c len = replicate (len - (c `minus` 'A' + 1)) ' '

listChars :: Char -> [Char]
listChars d = (enumFromTo 'A' d) ++ (enumFromThenTo (previousChar d 1)(previousChar d 2) 'A')

previousChar :: Char -> Int -> Char
previousChar 'A' _ = 'Z'
previousChar d pos = (chr ((ord d) - pos))