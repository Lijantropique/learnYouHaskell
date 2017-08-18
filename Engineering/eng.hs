{- ==========================
Project: Process Engineering Calculations with Haskell
========================== -}

-- TODO Add dispatcher: should call the functions and verify unitsCheck
-- TODO Once dispatcher is available, simplify area and velocity (no unit check)

unitsCheck :: (RealFloat a) => (String, Int) -> a
unitsCheck (units, group)
    |not (units `elem` [fst x|x<-(validU!!group)])   = error "Verify flow units"
    |otherwise = head [snd x | x <- (validU!!group), fst x == units]
    where
        flowU = [("gpm",6.3090199463E-5),("MGD",4.3812638516E-5),("lpm",1/6000),("m3/s",1)]
        lengthU = [("in",0.3048/12),("ft",0.3048),("mm",0.001),("m",1)]
        fluxU = [("GFD",4.371595316108E-7),("LMH",2.7777778-7),("m/s",1)]
        validU = [flowU, lengthU, fluxU]
        -- Add list with error messages

area :: (RealFloat a) => (a, String) -> a
area (diam, "")  = error "Include units"
area (diam, units)
    |diam <=0   = error "Pipe diameter MUST be greater than 0"
    |otherwise  = (pi/4) * (diam*factor)^2
    where
        factor  = unitsCheck (units)

velocity :: (RealFloat a) => ((a, String) , (a, String) ) -> a
velocity ((flow, flowU), (diam, diamU))
    |flow <=0   = error "Flow MUST bre greater than 0"
    |diam <=0   = error "Pipe diameter MUST be greater than 0"
    |otherwise  = (flow*factorU)/(area (diam, diamU))
    where
        factorU = unitsCheck (units)

-- dispatcher 
