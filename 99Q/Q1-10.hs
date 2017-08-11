-- Problem 1
myLast:: [a] -> a
myLast [] = error "Empty list"
myLast (lt:[]) = lt
myLast (_:lt) = myLast lt

myLast':: [a] -> a
myLast' [] = error "Empty list"
myLast' xs = xs !! (length xs -1)


-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast (lt:[]) = error "Only one element"
myButLast (blt:lt:[]) = blt
myButLast (_:lt) = myButLast lt

myButLast':: [a] -> a
myButLast' [] = error "Empty list"
myButLast' (lt:[]) = error "Only one element"
myButLast' xs = xs !! (length xs -2)

myButLast'' :: [a] -> a
myButLast'' [] = error "Empty list"
myButLast'' (lt:[]) = error "Only one element"
myButLast'' xs@(x:t)
    |length xs ==1 = x
    |otherwise =  myButLast t

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs idx = xs!!(idx-1)

elementAt' :: [a] -> Int -> a
elementAt' xs@(x:t) idx
    |idx < 1    = error "Minimum index is 1"
    |idx == 1   = x
    |idx <= length xs = elementAt t (idx -1)
    |otherwise  = error "Index greater than list length"

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = sum[1|x<-xs]

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' lst = reverse' lst []
    where
        reverse' [] reversed = reversed
        reverse' (x:xs) reversed = reverse' xs (x:reversed)

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == (reverse xs)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' (x:xs)
    |null xs = True
    |otherwise = (x == last xs) && isPalindrome' (init xs)


-- Problem 7
-- Wait until learn how to declare new datatypes
