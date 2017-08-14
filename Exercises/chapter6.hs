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
