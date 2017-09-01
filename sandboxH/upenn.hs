foo :: Integer -> Integer
foo 0 = 16
foo 1
    |"Haskell" > "C++"  = 3
    |otherwise          = 4
foo n
    |n < 0              = 0
    |n `mod` 17 == 2    = (-43)
    |otherwise          = n + 3


isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0


foo2 :: Int -> Int -> Int -> Int
foo2 x y z  = x + y + z

multplieEveryTwo xs =
    let
        inv = reverse xs
        in reverse$ multp inv
        where
            multp [] = []
            multp [x] = [x]
            multp (x:(y:zs)) = x:2*y:multp zs
