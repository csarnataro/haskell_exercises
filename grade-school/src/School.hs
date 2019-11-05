module School (
  School, 
  add, 
  empty, 
  grade, 
  sorted,
  makeClass) where

import Data.List (sort, groupBy)


type School = [(Int, String)]

add :: Int -> String -> School -> School
add gradeNum student [] = [(gradeNum, student)]
add gradeNum student school = (gradeNum, student) : school

empty :: School
empty = [] -- :: [(Int, String)]

grade :: Int -> School -> [String]
grade _ [] = []
grade gradeNum school = snd $ head $ filter (\x -> fst x == gradeNum) (sorted school)

sorted :: School -> [(Int, [String])]
sorted [] = []
sorted school =
  concat 
    $ map makeClass
    $ groupBy (\x y -> fst x == fst y) 
    $ sort school

makeClass :: [(Int, String)] -> [(Int, [String])]
makeClass [] = error "Empty list"
makeClass (x:xs) = [(fst x, snd x : map snd xs)]