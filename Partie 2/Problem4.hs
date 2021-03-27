{-# OPTIONS_GHC -Wall #-}

import EL

interp :: Prop -> [World]
interp "a1" = [10, 12, 13, 14]
interp "b1" = [01, 21, 31, 41]
interp "a2" = [20, 21, 23, 24]
interp "b2" = [02, 12, 32, 42]
interp "a3" = [30, 31, 32, 34]
interp "b3" = [03, 13, 23, 43]
interp "a4" = [40, 41, 42, 43]
interp "b4" = [04, 14, 24, 34]
interp _    = [] 

indis :: Agent -> World -> [World]
indis "a" 01 = [01, 02, 03, 04]
indis "a" 02 = [01, 02, 03, 04]
indis "a" 03 = [01, 02, 03, 04]
indis "a" 04 = [01, 02, 03, 04]

indis "b" 10 = [10, 20, 30, 40]
indis "b" 20 = [10, 20, 30, 40]
indis "b" 30 = [10, 20, 30, 40]
indis "b" 40 = [10, 20, 30, 40]

indis "a" 10 = [10, 12, 13, 14]
indis "a" 12 = [10, 12, 13, 14]
indis "a" 13 = [10, 12, 13, 14]
indis "a" 14 = [10, 12, 13, 14]

indis "b" 01 = [01, 21, 31, 41]
indis "b" 21 = [01, 21, 31, 41]
indis "b" 31 = [01, 21, 31, 41]
indis "b" 41 = [01, 21, 31, 41]

indis "a" 20 = [20, 21, 23, 24]
indis "a" 21 = [20, 21, 23, 24]
indis "a" 23 = [20, 21, 23, 24]
indis "a" 24 = [20, 21, 23, 24]

indis "b" 02 = [02, 12, 32, 42]
indis "b" 12 = [02, 12, 32, 42]
indis "b" 32 = [02, 12, 32, 42]
indis "b" 42 = [02, 12, 32, 42]

indis "a" 30 = [30, 31, 32, 34]
indis "a" 31 = [30, 31, 32, 34]
indis "a" 32 = [30, 31, 32, 34]
indis "a" 34 = [30, 31, 32, 34]

indis "b" 03 = [03, 13, 23, 43]
indis "b" 13 = [03, 13, 23, 43]
indis "b" 23 = [03, 13, 23, 43]
indis "b" 43 = [03, 13, 23, 43]

indis "a" 40 = [40, 41, 42, 43]
indis "a" 41 = [40, 41, 42, 43]
indis "a" 42 = [40, 41, 42, 43]
indis "a" 43 = [40, 41, 42, 43]

indis "b" 04 = [04, 14, 24, 34]
indis "b" 14 = [04, 14, 24, 34]
indis "b" 24 = [04, 14, 24, 34]
indis "b" 34 = [04, 14, 24, 34]

indis _ _ = []

s0 :: EpiState
s0 = (interp, indis, 11)

anneIgn :: EpiFormula
anneIgn = 

billIgnorance :: EpiFormula
billIgnorance = 

problem4 :: EpiFormula
problem4 = 