-- Loading Modules
import Data.List -> import all functions from this module
-- import Data.List(nub, sort) -> import only nub and sort
-- import Data.List hiding (nub, sort) -> import everything but nub and sort
-- import qualified Data.List as M -> import everything but to cxall the function it would be required to use M.function (useful when two modules have the same name for a f)


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
