-- Loading Modules
import Data.List -- -> import all functions from this module
-- import Data.List(nub, sort) -> import only nub and sort
-- import Data.List hiding (nub, sort) -> import everything but nub and sort
-- import qualified Data.List as M -> import everything but to cxall the function it would be required to use M.function (useful when two modules have the same name for a f)
import Data.Function
import Data.Char
import qualified Data.Map as Map
import Geometry
import qualified Geometry2.Cube as Cube

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- Data.List
joinWords :: [[Char]] -> [Char]
joinWords listWords = foldl1 (++) $ intersperse " " listWords
-- same as: intercalate " " listWords
-- same as: unowrds listWords

addPoly :: (Num a) => [[a]] -> [a]
addPoly listPoly = map sum $ transpose listPoly

-- Data.Char
-- encode :: Int -> [Char] -> [Char]
encode 0 str = str
encode shift str = map chr $ map (+shift) $ map ord str
-- encode shift str = map  (chr . (+shift) . ord) str -- -> applied to each letter

-- Data.Map
list = [("oscar",1),("javier",2),("jr",3),("jrico",4)]
findKey key xs =
    let
        raw = filter (\x-> fst x==key) xs
        in if null raw then -1 else snd $ raw!!0

findKey' key [] = Nothing
findKey' key ((k,v):xs)
    |k==key     = Just v
    |otherwise  = findKey' key xs

findKey'' key =  foldr (\(k,v) acc -> if key==k then Just v else acc) Nothing
