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

indis "a" 01 = [01, 02, 03, 04]

indis "b" 10 = [10, 20, 30, 40]

indis "a" 10 = [10, 12, 13, 14]
indis "a" 12 = [10, 12, 13, 14]

indis "b" 01 = [01, 21, 31, 41]
indis "b" 21 = [01, 21, 31, 41]

indis "a" 21 = [20, 21, 23, 24]
indis "a" 23 = [20, 21, 23, 24]

indis "b" 12 = [02, 12, 32, 42]
indis "b" 32 = [02, 12, 32, 42]

indis "a" 32 = [30, 31, 32, 34]
indis "a" 34 = [30, 31, 32, 34]

indis "b" 23 = [03, 13, 23, 43]
indis "b" 43 = [03, 13, 23, 43]

indis "a" 43 = [40, 41, 42, 43]

indis "b" 34 = [04, 14, 24, 34]

indis _ _ = []

s0 :: EpiState
s0 = (interp, indis, 12)

anneIgn :: EpiFormula
anneIgn = 

billIgnorance :: EpiFormula
billIgnorance = 

problem4 :: EpiFormula
problem4 = 