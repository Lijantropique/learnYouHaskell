import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let
        (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
        in (firstCoin, secondCoin, thirdCoin)

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a],g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let
        (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
        in (value:restOfList, finalGen)

finiteRandoms' :: (RandomGen g) => Int -> g -> ([Int],g)
finiteRandoms' 0 gen = ([], gen)
finiteRandoms' n gen =
    let
        (value, newGen) = random gen
        restOfList = take (n-1) $ randoms newGen :: [Int]
        in (value:restOfList, newGen)
        -- the problem is it would use twice the same numbers
        
