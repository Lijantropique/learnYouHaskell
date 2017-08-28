import Control.Monad
main = do
     colors <- forM [1,2,3,4] (\a -> do
         putStrLn $ "Which color do you associate with number "++ show a ++ "?"
         color <- getLine
         return color)
     putStrLn "The colors that you associated are:"
     mapM putStrLn $ temp colors
     where
         temp lst = map (\(a,b)->show a++"-"++b) $ zip [1.. length lst] lst
