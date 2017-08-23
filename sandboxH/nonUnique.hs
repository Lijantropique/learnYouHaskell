import qualified Data.List as DL
import Data.Char
import T
-- checkio :: (Integral a) => [a] -> [a]
checkio [] = error "Empty list"
checkio xs = foldr validate [] xs
    where
        validate x acc = if (length $ x `DL.elemIndices` xs) > 1
                        then x:acc
                        else acc

checkio' [] = []
checkio' xs = filter (\x-> (length $ x `DL.elemIndices` xs) > 1) xs

checkio'' xs = filter (\x-> length x>1) $ map (\x->x `DL.elemIndices` xs) xs

-- the most wanted letter
checkio''' xs = let
    rawString = DL.sort $ map toLower $ filter isAlpha xs
    rawGroup = foldl1 (\acc x-> if length x > length acc then x else acc) $ DL.group rawString
    in rawGroup!!0

test z
    |all (True==) $ map (\(i,a)-> checkio''' i == a) z = "All test completed"
    |otherwise = "problem"
