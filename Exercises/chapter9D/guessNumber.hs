import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO()
askForNumber gen = do
    let
        (randomNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 10 I am thinking of?"
    numberStr <- getLine
    when (not $ null numberStr) $ do
        let
            number = fst $ (reads numberStr)!!0
        if number == -1
            then return ()
            else do
                if number == randomNumber
                    then putStrLn "You are correct"
                    else putStrLn $ "Sorry the number was" ++ show randomNumber
                askForNumber newGen
