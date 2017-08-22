-- Algebraic data types intro
module Chapter8
    ( Point(..)
    , Shape(Circle) -- it owuld only export Circle, Rectangle would be disallowed in other modules (available in ghci though)
    , surface
    , baseCircle
    , baseRect
    , Person(..)
    ) where

import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi *  r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2-x1) * (abs $ y2-y1)

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r
baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Record Syntax
-- Person = firstName lastName age height phoneNumber favoriteIceCream
-- data Person = Person String String Int Float String String deriving (Show)
data Person = Person    {fisrtName :: String
                        ,lastName :: String
                        ,age :: Int
                        ,height :: Float
                        ,phoneNumber :: String
                        ,flavor:: String
                        }deriving (Show)


-- type parameters
data Vector a = Vector a a a deriving (Show)

vplus:: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
-- Based on the input, it would a a type Vector int, Vector Integer, Vector Float etc


-- Derived instances
data Day = Monday | Tuesday | Wednesday | Thrusday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Unit =  Unit {name::String, factor::Double} deriving (Show, Read, Ord, Eq)
data U = U {val::Double, unit::Unit} deriving (Show)

mm' = Unit "mm" 0.001
ft' = Unit "ft" 0.3048
in' = Unit "ft" (0.3048/12)
gpm' = Unit "gpm" 6.309E-5

lengthU = [mm',ft',in']
flowU = [gpm']

areaM :: U -> Double
areaM (U v u)
    |u `elem` lengthU = (pi/4) * (v* factor u)^2
    |otherwise = error "Units not valid for function"

-- funUnits ::
parseUnits (U v u) group
    |u `elem` group = v * factor u
    |otherwise =  error "Units not valid for function"

areaM' (U v u) =  (pi/4) * v'^2
    where
        v' = parseUnits (U v u) lengthU

velM flow diam=  f'/ areaM' diam
    where
        f' = parseUnits flow flowU

velM' flow diam=  f'/ ((pi/4) * v'^2)
    where
        f' = parseUnits flow flowU
        v' = parseUnits diam lengthU

data Pipe = Pipe { di::U, material::String} deriving (Show)

newPipe :: Double -> Unit -> String -> Pipe
newPipe di' u' m'
    |u' `elem` lengthU = Pipe { di=(U di' u'), material=m'}
    |otherwise = error "Units not valid"
    -- map (\(x, g) -> x `elem` g) $ zip [u1, u2] [g1, g2]
    -- all (==True) $ map (\(x,g)->x `elem` g) $ zip [1,2,3] [[1,2,3],[4,5,6,2],[3,4,6]]

areaM'' (Pipe di _) = areaM' di


data Foo = Type1 | Type2 deriving (Show)
a = Type1

plus :: (Num a) => Foo -> a ->a
plus f a = 1+ a
