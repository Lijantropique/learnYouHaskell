{- ==========================
Project: Process Engineering Calculations with Haskell
========================== -}
import qualified Units as U

-- TODO Add dispatcher: should call the functions and verify unitsCheck
-- TODO Once dispatcher is available, simplify area and velocity (no unit check)


area :: (RealFloat a) => a -> a
area diam
    |diam <=0   = error "Pipe diameter MUST be greater than 0"
    |otherwise  = (pi/4) * diam^2

velocity :: (RealFloat a) => (a, a) -> a
velocity (flow, diam)
    |flow <=0   = error "Flow MUST bre greater than 0"
    |diam <=0   = error "Pipe diameter MUST be greater than 0"
    |otherwise  = flow / (area diam)

-- dispatcher
