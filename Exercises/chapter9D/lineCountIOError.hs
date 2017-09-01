import System.Environment
import System.IO
import System.IO.Error

main :: IO ()
main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The file '" ++ fileName ++ "' has " ++ show (length $ lines contents) ++ " lines!"

handler :: IOError -> IO ()
handler e
    |isDoesNotExistError e = putStrLn "The file doesn't exist"
    |isUserError e = putStrLn "Did you include the file name?"
    |otherwise = ioError e
    -- |otherwise = putStrLn "Whoops, had some trouble"
