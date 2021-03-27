{-# OPTIONS_GHC -Wall #-}

import EL

interp :: Prop -> [World]
interp "a0" = [01]
interp "b0" = [10]
interp "a1" = [10, 12]
interp "b1" = [01, 21]
interp "a2" = [21, 23]
interp "b2" = [12, 32]
interp "a3" = [32, 34]
interp "b3" = [23, 43]
interp "a4" = [43]
interp "b4" = [34]
interp _    = [] 

indis :: Agent -> World -> [World]

indis "b" 01 = [01]
indis "b" 10 = [10]

indis "a" 10 = [10, 12]
indis "a" 12 = [10, 12]
indis "b" 01 = [01, 21]
indis "b" 21 = [01, 21]

indis "a" 21 = [21, 23]
indis "a" 23 = [21, 23]
indis "b" 12 = [12, 32]
indis "b" 32 = [12, 32]

indis "a" 32 = [32, 34]
indis "a" 34 = [32, 34]
indis "b" 23 = [23, 43]
indis "b" 43 = [23, 43]

indis "a" 43 = [43]
indis "b" 34 = [34]

indis _ _ = []

s0 :: EpiState
s0 = (interp, indis, 12)

anneIgn :: EpiFormula
anneIgn = 

billIgnorance :: EpiFormula
billIgnorance = 

problem4 :: EpiFormula
problem4 = 