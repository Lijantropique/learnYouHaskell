-- Pattern matching
lucky :: (Num a, Eq a) => a -> String
lucky 7 = "Lucky Number seven!"
lucky x = "Sorry, you're out of luck!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

velocity :: (Num a, Fractional a) => (a, a) -> a
velocity (flow, area) = flow/area

triplets :: (a,b,c) -> (b,c,a)
triplets (a,b,c) = (b,c,a)

head' :: [a]->a
head' [] = error "Error"
head' (x:[]) = x
head' (x:y:z) = x


length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

a = [[1,2,3],[4,5],[6],[10,20,30,40]]
head2 :: [[t]] -> [t]
head2 xs = [x | (x:y:_) <- xs]

capital :: String -> String
capital "" = "Empty String"
capital todo@(x:xs) = "The first letter of " ++ todo ++ " is " ++ [x]

suma :: (Num a) => [[a]] -> [(a,a)]
suma xs = [(sum todo,x)| todo@(x:_)<- xs]

-- q: what would be the bets way to check input?
velocity2 :: (Num a, Fractional a, Eq a) => [a] -> a
velocity2 [] = error "Not enough arguments provided"
velocity2 (flow:[]) = error "Not enough arguments provided"
velocity2 (flow:area:[]) = error "Not enough arguments provided"
velocity2 (flow:area:[1]) = (flow/area)*100
velocity2 (flow:area:[2]) = (flow/area)*200
velocity2 xs = error " The only options availbale are 1 or 2"

-- #Todo with guards
