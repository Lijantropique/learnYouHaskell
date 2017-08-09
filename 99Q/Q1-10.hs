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
