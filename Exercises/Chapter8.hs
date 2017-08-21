-- Algebraic data types intro
module Chapter8
    ( Point(..)
    , Shape(Circle) -- it owuld only export Circle, Rectangle would be disallowed in other modules (available in ghci though)
    , surface
    , baseCircle
    , baseRect
    , Person(..)
    ) where

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
