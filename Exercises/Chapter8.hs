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
import Data.List

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
plus f a = a+1

-- type synonyms
type Name = String
type PhoneNumber = String
type PhoneBook = [(Name,PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
    [("Betty","555-2938")
    ,("lucille","205-2928")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]
searchAssocL :: (Eq k) => k -> AssocList k v -> Maybe v
searchAssocL key assocL  = case find (\(k,v)-> key== k) assocL of
    -- Nothing -> "message" not possible with Maybe, look below for Either
    Nothing -> Nothing
    Just (k,v) -> Just v

type IntMap v = Map.Map Int v
intmapStr :: IntMap String
intmapStr = Map.fromList [(1, "one"),(2,"two")]

intmapInt :: IntMap Int
intmapInt = Map.fromList [(1, 10),(2,20)]


data LockerState = Taken | Free deriving (Show, Eq)
type Code = Int
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNo map = case Map.lookup lockerNo map of
    Nothing -> Left $ "Locker number " ++ show lockerNo ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker number " ++ show lockerNo ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,1))
    ,(200,(Free,2))
    ,(300,(Free,3))
    ]

-- Recursive data structures
-- data List a = Empty | Pons a (List a) deriving (Show, Read, Eq, Ord)

infixr 5 :-:
-- infixr -> right associative (4:-:3:-:2) = (4:-:(3:-:2))
-- infixl -> left associative (4:-:3:-:2) = ((4:-:3):-:2)
-- the number states the fixity :
-- 5*4+3 -> (5*4)+3, because *->infixl 7 * and +-> infixl *  6
data List' a = Empty | a :-: (List' a) deriving (Show, Read, Eq, Ord)

infix 5 .++
(.++)::List' a -> List' a -> List' a
Empty .++ ys = ys
(x:-:xs) .++ ys = x:-: (xs .++ ys)
