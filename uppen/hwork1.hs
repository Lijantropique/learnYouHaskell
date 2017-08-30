{-# OPTIONS_GHC -Wall #-}
-- ====|
--    Homework 1 from 'Introduction to Haskell (Spring 2013)'
--    August 2017
-- |====


-- ===| Exercise 1 |=== --
-- Find the digits of a number
toDigits :: Integer -> [Integer]
toDigits n
    |n<=0   = []
    |otherwise = (toDigits (n `div` 10)) ++ [n `rem` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- ===| Exercise 2 |=== --
-- Double every other number
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = map double $ addPosition lst
    where
        double :: (Int, Integer) -> Integer
        double (pos,x) = if odd pos then x else 2*x
        addPosition :: [Integer] -> [(Int, Integer)]
        addPosition x = zip [length x, length x-1..1] x

-- ===| Exercise 3 |=== --
-- Sum all the digits
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum $ toDigits x) + (sumDigits xs)

-- ===| Exercise 4 |=== --
-- validate a credit card number
validate :: Integer -> Bool
validate n
    |nTransformed `rem` 10 == 0     = True
    |otherwise                      = False
    where
        nTransformed = sumDigits $ doubleEveryOther $ toDigits n
