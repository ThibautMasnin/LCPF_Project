{-# OPTIONS_GHC -Wall #-}

module Problem4 where
import EL

{- interp : prend une proposition en argument et renvoie 
la liste de mondes possibles où la proposition est vraie-}
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

{-indis : prend un agent i et un monde possible w en arguments et renvoie 
la liste de mondes possibles qui sont indiscernables du monde w pour l’agent i-}
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

{-s0 : définition complète de l’état épistémique initial du problème-}
s0 :: EpiState
s0 = (interp, indis, 12)

{-aliceIgn : Exprime l’ignorance d’Alice sur son état-}
anneIgn :: EpiFormula
anneIgn = 
    Or 
    (Or 
    (And (Not (Knows "a" (Var "b0"))) (Not (Knows "a" (Not (Var "b0"))))) 
    (And (Not (Knows "a" (Var "b1"))) (Not (Knows "a" (Not (Var "b1"))))))
    (Or 
    (And (Not (Knows "a" (Var "b2"))) (Not (Knows "a" (Not (Var "b2"))))) 
    (Or 
    (And (Not (Knows "a" (Var "b3"))) (Not (Knows "a" (Not (Var "b3")))))
    (And (Not (Knows "a" (Var "b4"))) (Not (Knows "a" (Not (Var "b4")))))))

{-billIgn : Exprime l’ignorance de Bill sur son état-}
billIgn :: EpiFormula
billIgn =
    Or  
    (Or 
    (And (Not (Knows "b" (Var "a0"))) (Not (Knows "b" (Not (Var "a0"))))) 
    (And (Not (Knows "b" (Var "a1"))) (Not (Knows "b" (Not (Var "a1"))))))
    (Or 
    (And (Not (Knows "b" (Var "a2"))) (Not (Knows "b" (Not (Var "a2"))))) 
    (Or 
    (And (Not (Knows "a" (Var "a3"))) (Not (Knows "a" (Not (Var "a3")))))
    (And (Not (Knows "b" (Var "a4"))) (Not (Knows "b" (Not (Var "a4")))))))

{-problem4 : Exprime le problème 4 dans sa totalité-}
problem4 :: EpiFormula
problem4 = And 
        (And anneIgn billIgn) 
        (After anneIgn 
        (After billIgn (Not anneIgn)))

testProblem4 :: Bool
testProblem4 = epiSat s0 problem4