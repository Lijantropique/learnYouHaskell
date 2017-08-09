{- ==========================
Project: Process Engineering Calculations with Haskell
========================== -}

-- STYLE??

area :: (RealFloat a) => (a, String) -> a
area (diam, "")  = error "Include units"
area (diam, units)
    |diam <=0                   = error "Pipe diameter MUST be greater than 0"
    |not (units `elem` validU)  = error "Only valid units are allowed"
    |otherwise = (pi/4) * diam^2
    where
        validU = ["in","ft","mm","m"]

velocity :: (RealFloat a) => ((a, String) , (a, String) ) -> a
velocity ((flow, flowU), (diam, diamU))
    |flow <=0   = error "Flow MUST bre greater than 0"
    |diam <=0   = error "Pipe diameter MUST be greater than 0"
    |otherwise  = flow/(area (diam, diamU))
