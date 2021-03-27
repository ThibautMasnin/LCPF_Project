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

indis "a" 01 = [01]
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
anneIgn = And   (And (Not(Knows "a" (Var "b0"))) (Not(Knows "a" (Not (Var "b2")))))
                (And (Not(Knows "a" (Var "b3"))) (Not(Knows "a" (Not (Var "b4")))))

billIgn :: EpiFormula
billIgn = And   (And (Not(Knows "b" (Var "a0"))) (Not(Knows "b" (Not (Var "a1")))))
                (And (Not(Knows "b" (Var "a3"))) (Not(Knows "b" (Not (Var "a4")))))

problem4 :: EpiFormula
problem4 = 