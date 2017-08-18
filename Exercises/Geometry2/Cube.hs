module Geometry2.Cube
( volume
, area
) where

import qualified Geometry2.Cuboid as Cuboid

volume :: Float -> Float
volume side = side^3

area :: Float -> Float
area side = Cuboid.area side side
