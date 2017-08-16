-- Curried functions
partial :: Int -> Int -> Int -> Int
partial x y z = x*y*z

double :: Int -> Int -> Int
double = partial 2

tenTimes :: Int -> Int
tenTimes = double 5

-- Q: curried functions vs partial application

-- Some higher-orderism is in order
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f l1 [] = []
zipWith' f [] l2 = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- Maps and filters
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

testList :: Num a => (a -> b) -> [[b]]
testList f = map (map f) [[1,2],[3,4,5,6],[7,8]]
a = map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
    |f x = x: filter' f xs
    |otherwise = filter' f xs

b = let
    notNull x = not (null x)
    in filter' notNull [[1,2,3],[],[4,5],[],[]]

quickSort :: (Ord a) => [a] -> [a]
quickSort []  =[]
quickSort (x:t) = smaller ++ [x] ++ bigger
    where
        smaller = quickSort (filter (<=x) t)
        bigger = quickSort (filter (>x) t)

largestDivisible = head (filter divisible [100000,99999..])
    where
        divisible x = (x `mod` 3829) == 0


oddSquares = sum (filter (<10000) (filter odd (map (^2) [1..10000])))
oddSquares' = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x = x: chain(collatz x)
    where
        collatz n
            |even n = n `div` 2
            |otherwise = n*3+1

collatzGT15 = length (filter valid (map chain [1..100]))
    where
        valid list = length list >15


-- last Curried example map
c = myFun 5
    where
        myFun = (map (*) [0..])!!6
        -- (map (*) [0..]) -> [0*, 1*, 2*, 3*..] all functions of 1 parameter
        --  because * requires two aparameters
        --  myFun 5 == (6*)5 == 6*5


-- Lambdas
collatzGT15' = length (filter (\xs -> length xs > 15) (map chain [1..100]))
vol = map (\(d,l)->(pi/4)*d^2*l) [(2,10),(3,11), (4,12)]

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x


-- Only folds and horses
-- left fold
-- NOTE: left fold MUST have accumulator as first parameter of binary function
sum' :: (Num a)=> [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x-> x==y || acc) False ys

-- right fold
-- NOTE: right fold MUST have accumulator as second parameter of binary function

map'' :: (a -> b) -> [a] -> [b]
map'' f ys = foldr (\x acc->f x:acc) [] ys
-- with left fold  map'' f ys = foldl (\x acc->acc++[f x]) [] ys
-- but ++ is more expensive than :

-- NOTE right fold works on infinite lists


maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc x-> max acc x)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x->x:acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

product'' :: (Num a) => [a] -> a
product'' = foldl1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldl (\acc x->if f x then x:acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x acc ->x)

last' :: [a] -> a
last' = foldl1 (\acc x ->x)

-- foldr f [1,2,3] z -> f 1 (f 2 (f 3 z))
-- foldl g [1,2,3] z -> g (g (g 1 z) 2) 3


-- Function application with $

-- f a $ b $ c -> f (a (b c))
-- collatzGT15' = length (filter (\xs -> length xs > 15) (map chain [1..100]))
collatzGT15'' = length $ filter (\xs -> length xs > 15) $ map chain [1..100]
