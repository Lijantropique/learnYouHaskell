-- Recusion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail   = x
    |otherwise      = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum' xs)

replicate':: Int -> a -> [a]
replicate' n item
    |n <= 0     = []
    |otherwise  = item:(replicate' (n-1) item)

take' :: Int -> [a] -> [a]
take' n xs@(x:t)
    |n <= 0 || null xs = []
    |otherwise  = x:take' (n-1) t

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = a:repeat' a

zip' :: [a] -> [a] -> [(a,a)]
zip' _ [] = []
zip' [] _ = []
zip' (x:tx) (y:ty) = (x,y):zip' tx ty

zip'' :: [a] -> [a] -> [(a,a)]
zip'' xs ys
    |null xs || null ys = []
    |otherwise = (head xs,head ys): zip' (tail xs) (tail ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
    |e == x = True
    |otherwise = e `elem'` xs

-- Quick, sort!
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:t) = quickSort [i | i <- t,i<=x] ++ [x] ++ quickSort [i | i <- t,i>x]

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:t) = smaller ++ [x] ++ bigger
    where
        smaller = quickSort [i | i <- t,i<=x]
        bigger = quickSort [i | i <- t,i>x]

quickSort'' :: (Ord a) => [a] -> [a]
quickSort'' [] = []
quickSort'' (x:t) =
    let
        smaller = quickSort [i | i <- t,i<=x]
        bigger = quickSort [i | i <- t,i>x]
        in smaller ++ [x] ++ bigger
