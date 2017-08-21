module Units
    ( Units
    , newUnit
    ) where

import qualified Data.Map as M
flowU = M.fromList [("gpm",6.3090199463E-5),("MGD",4.3812638516E-5),("lpm",1/6000),("m3/s",1)]
lengthU = M.fromList [("in",0.3048/12),("ft",0.3048),("mm",0.001),("m",1)]
fluxU = M.fromList [("GFD",4.371595316108E-7),("LMH",2.7777778-7),("m/s",1)]
validU = [flowU, lengthU, fluxU]

data Units = Units  {value :: Float
                    ,name :: String
                    } deriving (Show)


newUnit :: Float -> String -> Int -> Units
newUnit val unit groupID
    |not $ M.member unit $ validU!!0 = error "Verify Units"
    |otherwise = Units {value=val, name=unit}



unitsCheck :: (RealFloat a) => (String, Int) -> a
unitsCheck (units, group)
    |not (units `elem` [fst x|x<-(validU!!group)])   = error "Verify flow units"
    |otherwise = head [snd x | x <- (validU!!group), fst x == units]
    where
        flowU = [("gpm",6.3090199463E-5),("MGD",4.3812638516E-5),("lpm",1/6000),("m3/s",1)]
        lengthU = [("in",0.3048/12),("ft",0.3048),("mm",0.001),("m",1)]
        fluxU = [("GFD",4.371595316108E-7),("LMH",2.7777778-7),("m/s",1)]
        validU = [flowU, lengthU, fluxU]
