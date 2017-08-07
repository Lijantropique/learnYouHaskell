-- Believe the type
-- removePunctuation :: [Char] -> [Char]
removePunctuation :: String -> String
removePunctuation string = [x | x<- string, x `elem` ['a'..'z']++['A'..'Z']++[' ']]

velocity :: Float -> Float -> Float
velocity flow area = flow/area

-- Type variables
-- Typeclasses 101
