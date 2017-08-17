-- The most wanted letter
-- htpps://py.checkio.org/mission/most-wanted-letter/
import Data.List
import Data.Char

-- first approach
lowerL letter = if letter `elem` ['A'..'Z']
                then head [snd item | item <- zip ['A'..'Z']['a'..'z'], letter == fst item]
                else letter
lowerS string = [lowerL x | x <- string, x `elem` ['a'..'z']++['A'..'Z']]
count string letter = sum[1 | x <- string, x==letter]

checkio string = snd (maximum[(count string x,x)| x <- lowerS string])


-- second approach
checkio' :: [Char] -> Char
checkio' xs = let
    rawString = sort $ map toLower $ filter isAlpha xs
    rawGroup = map (\x->(length x,x!!0)) $ group rawString
    n = fst $ maximum rawGroup
    in snd $ (filter (\(a,b) -> a >= n) rawGroup) !!0

-- third approach
-- checkio'' :: [Char] -> Char
checkio'' xs = let
    rawString = sort $ map toLower $ filter isAlpha xs
    rawGroup = foldl1 (\acc x-> if length x > length acc then x else acc) $ group rawString
    in rawGroup!!0
