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
