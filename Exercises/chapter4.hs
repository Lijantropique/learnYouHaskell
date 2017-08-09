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

-- q: what would be the best way to check input? pattern, guards, if?
velocity2 :: (Num a, Fractional a, Eq a) => [a] -> a
velocity2 [] = error "Not enough arguments provided"
velocity2 (flow:[]) = error "Not enough arguments provided"
velocity2 (flow:area:[]) = error "Not enough arguments provided"
velocity2 (flow:area:[1]) = (flow/area)*100
velocity2 (flow:area:[2]) = (flow/area)*200
velocity2 xs = error " The only options availbale are 1 or 2"

-- Guards, guards! & Where!?
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    |bmi <= skinny  = "You're underweight"
    |bmi <= normal  = "You're normal"
    |bmi <= fat     = "You're fat"
    |otherwise      = "You're whale"
    where   bmi = weight/ height^2
            (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials "" ln = error "Just ln"
initials fn "" = error "Just fn"
initials (f:fn) (l:ln) = [f] ++ "." ++ [l] ++"."


foo x = x   -- global definition of foo"    
makeup :: (Integral a, Ord a) => a -> String
makeup 0
    |foo 0 < 1 = "Foo less zero"
makeup 1
    |foo 1 <1 = "Foo less one"
makeup x
    |foo x >1 = "Foo is greater than one"
    |otherwise = "Foo is strange"
