{-# OPTIONS_GHC -Wall #-}

import EL

interp :: Prop -> [World]
interp "as" = [10, 11]
interp "bs" = [01, 11]
interp _    = [] 

indis :: Agent -> World -> [World]
indis "a" 00 = [00, 10]
indis "b" 00 = [00, 01]
indis "a" 01 = [01, 11]
indis "b" 01 = [01, 00]
indis "a" 10 = [10, 00]
indis "b" 10 = [10, 11]
indis "a" 11 = [11, 01]
indis "b" 11 = [11, 10]
indis _ _ = []

s0 :: EpiState
s0 = (interp, indis, 01)

fatherAnn :: EpiFormula
fatherAnn = Or (Var "as") (Or (Var "bs") (And (Var "as") (Var "bs"))) 

aliceIgn :: EpiFormula
aliceIgn = And (Not(Knows "a" (Var "as"))) (Not(Knows "a" (Not (Var "as"))))

bobIgn :: EpiFormula
bobIgn = And (Not(Knows "b" (Var "bs"))) (Not(Knows "b" (Not (Var "bs"))))

problem1 :: EpiFormula
problem1 = 
    And (And aliceIgn bobIgn)
        (After  fatherAnn 
                (And (And aliceIgn (Not bobIgn)) (After (Not bobIgn) (Not aliceIgn)))
        )