{- ==========================
Project: Process Engineering Calculations with Haskell
========================== -}
import qualified Hydraulics as H
p = H.newPipe 0.1023 30.48 4.572E-5 "ss"
f = H.newFluid 0.0126 1000 0.001 "water"
dp =  H.pressureDrop f p
