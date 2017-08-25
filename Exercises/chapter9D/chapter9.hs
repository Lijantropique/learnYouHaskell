-- Hello world
main = do
    putStrLn "hello, what is your name?"
    name <- getLine
    putStrLn $ tellFortune name

tellFortune :: String -> String
tellFortune "Oscar J" = "we are both alike"
tellFortune _ = "Hey, who are you?"
