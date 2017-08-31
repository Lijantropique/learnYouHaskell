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

-- ===| Exercise 5 |=== --
--  The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hannoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hannoi 1 a b _ = [(a, b)]
hannoi n a b c = hannoi (n-1) a c b ++ [(a, b)] ++ hannoi (n-1) c b a

-- ===| Exercise 6 |=== --
--  The Towers of Hanoi, 4 pegs
hannoi4P :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hannoi4P 1 a b _ _ = [(a, b)]
hannoi4P 2 a b c _ = [(a, c), (a, b), (c, b)]
hannoi4P 3 a b c d = [(a, c), (a, d), (a, b), (d,b),(c,b) ]
-- hannoi4P n a b c d = hannoi4P (n-2) a c b d ++ [(a, d), (a, b), (d, b)] ++ hannoi4P (n-2) c b a d
hannoi4P n a b c d = hannoi4P (n-2) a c b d ++ hannoi4P 2 a b d c ++ hannoi4P (n-2) c b a d
