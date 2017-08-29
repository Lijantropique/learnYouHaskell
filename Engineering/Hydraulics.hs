module Hydraulics
    ( Pipe
    , Fluid
    , newPipe
    , newFluid
    , area
    , velocity
    , reynolds
    , frictionFactor
    , pressureDrop
    ) where

-- All units are SI
type Name = String

data Pipe = Pipe    {idP::Double
                    ,lengthP::Double
                    ,absRoughness::Double
                    ,nameP::Name}

instance Show Pipe where
    show (Pipe id' l' epsilon' m')= "The pipe has the following \
        \specifications:\n\t-material:" ++ m' ++ "\n\t-internal \
        \diameter:" ++ show id' ++ "\n\t-length:" ++ show l' ++ "\n\t-abs\
        \roughness:" ++ show epsilon'

data Fluid = Fluid  {flow::Double
                    ,density::Double
                    ,viscosity::Double
                    ,nameF::Name}

newPipe :: Double -> Double -> Double -> Name -> Pipe
newPipe id' l' epsilon' m'
    |all (>0) [id',l', epsilon']  = Pipe { idP=id', lengthP=l', absRoughness=epsilon', nameP=m'}
    |otherwise = error "internal diameter and length must be greater than zero"

newFluid :: Double -> Double -> Double -> Name -> Fluid
newFluid flow' rho' mu' m'
    |all (>0) [flow', rho', mu']  = Fluid { flow=flow', density=rho', viscosity=mu', nameF=m'}
    |otherwise = error "flow, density and viscosity must be greater than zero"

area :: Pipe -> Double
area pipe = (pi/4) * (idP pipe)^2

velocity :: Fluid -> Pipe -> Double
velocity fluid pipe = (flow fluid) / (area pipe)

relRoughness :: Pipe -> Double
-- Age factor = 1.2
relRoughness pipe = 1.2*(absRoughness pipe)/(idP pipe)

reynolds :: Fluid -> Pipe -> Double
reynolds fluid pipe = 4*(flow fluid)*(density fluid)/pi/(idP pipe)/(viscosity fluid)

frictionFactor :: Fluid -> Pipe -> Double --Fanning friction factor
frictionFactor fluid pipe = let
    nRe = reynolds fluid pipe
    relR = relRoughness pipe
    factorA = (2.457 * log(1/(((7/nRe)**0.9)+0.27*relR)))**16
    factorB = (37530/nRe)**16
    in 2*((((8/nRe)**12)+(1/(factorA+factorB)**1.5)))**(1/12)

pressureDrop :: Fluid -> Pipe -> Double
pressureDrop fluid pipe = let
    fanning = frictionFactor fluid pipe
    in fanning*32*(density fluid)*(lengthP pipe)*(flow fluid)^2/(pi^2)/(idP pipe)^5/1000

-- TODO: analyze pipe -> list with velocity, erosive velocity, pressure drop lineal\
-- pressuredrop per length pipe, elevation reynolds (and type of regime)
-- add units
-- read pipe/fluid from command line or from file?
-- add configuration file with age factor, erosive vel factor
-- add standard pipe sizes chart
-- create report
-- add GUI
