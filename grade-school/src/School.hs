module School (School, add, empty, grade, sorted) where

type School = [(Int, String)]

add :: Int -> String -> School -> School
add gradeNum student school = school : (gradeNum, student)

empty :: School
empty = [] -- :: [(Int, String)]

grade :: Int -> School -> [String]
grade gradeNum school = ["Aimee"] -- error "You need to implement this function."

sorted :: School -> [(Int, [String])]
sorted school = [(1, ["Aimee"])] -- error "You need to implement this function."
