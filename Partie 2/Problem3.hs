{-# OPTIONS_GHC -Wall #-}

import EL

interp :: Prop -> [World]
interp "as" = [100, 110, 101, 111]
interp "bs" = [010, 110, 011, 111]
interp "cs" = [001, 011, 101, 111]
interp _    = []

indis :: Agent -> World -> [World]

indis "a" 000 = [000, 100]
indis "b" 000 = [000, 010]
indis "c" 000 = [000, 001]

indis "a" 001 = [001, 101]
indis "b" 001 = [001, 011]
indis "c" 001 = [001, 000]

indis "a" 010 = [010, 110]
indis "b" 010 = [010, 000]
indis "c" 010 = [010, 011]

indis "a" 011 = [011, 111]
indis "b" 011 = [011, 001]
indis "c" 011 = [011, 010]

indis "a" 100 = [100, 000]
indis "b" 100 = [100, 110]
indis "c" 100 = [100, 101]

indis "a" 101 = [101, 001]
indis "b" 101 = [101, 111]
indis "c" 101 = [101, 100]

indis "a" 110 = [110, 010]
indis "b" 110 = [110, 100]
indis "c" 110 = [110, 111]

indis "a" 111 = [111, 011]
indis "b" 111 = [111, 101]
indis "c" 111 = [111, 110]

indis _ _ = []

s0 :: EpiState
s0 = (interp, indis, 111)

fatherAnn :: EpiFormula
fatherAnn = (Or (Var "as") (Or 
    (Var "bs") (Or 
    (Var "cs") (Or 
    (And (Var "as") (Var "bs")) (Or 
    (And (Var "as") (Var "cs")) (Or 
    (And (Var "bs") (Var "cs")) (And 
    (Var "as") (And 
    (Var "bs") (Var "cs")))))))))

aliceIgn :: EpiFormula
aliceIgn = And (Not (Knows "a" (Var "as"))) (Not (Knows "a" (Not (Var "as"))))
    
bobIgn :: EpiFormula
bobIgn = And (Not (Knows "b" (Var "bs"))) (Not (Knows "b" (Not (Var "bs"))))

carolineIgn :: EpiFormula
carolineIgn = And (Not (Knows "c" (Var "cs"))) (Not (Knows "c" (Not (Var "cs"))))

problem3 :: EpiFormula
problem3 = And (And aliceIgn (And bobIgn carolineIgn)) (After fatherAnn (After 
    (And aliceIgn (And bobIgn carolineIgn)) (After 
    (And aliceIgn (And bobIgn carolineIgn)) (And 
    (Not aliceIgn) (And (Not bobIgn) (Not carolineIgn))))))