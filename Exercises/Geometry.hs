module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0/3.0) * pi * (radius^3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius^2)

cubeVolume :: Float -> Float
cubeVolume side = side^3

cubeArea :: Float -> Float
cubeArea side = side * side

helper :: Float -> Float -> Float
helper l1 l2 = l1 * l2
