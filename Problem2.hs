{-# OPTIONS_GHC -Wall #-}

module Problem2 where
import EL

{- interp : prend une proposition en argument et renvoie 
la liste de mondes possibles où la proposition est vraie-}
interp :: Prop -> [World]
interp "as" = [10, 11]
interp "bs" = [01, 11]
interp _    = [] 

{-indis : prend un agent i et un monde possible w en arguments et renvoie 
la liste de mondes possibles qui sont indiscernables du monde w pour l’agent i-}
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

{-s0 : définition complète de l’état épistémique initial du problème-}
s0 :: EpiState
s0 = (interp, indis, 11)

{-fatherAnn : Exprime l’annonce du père-}
fatherAnn :: EpiFormula
fatherAnn = Or (Var "as") (Or (Var "bs") (And (Var "as") (Var "bs"))) 

{-aliceIgn : Exprime l’ignorance d’Alice sur son état-}
aliceIgn :: EpiFormula
aliceIgn = And (Not(Knows "a" (Var "as"))) (Not(Knows "a" (Not (Var "as"))))

{-bobIgn : Exprime l’ignorance de Bob sur son état-}
bobIgn :: EpiFormula
bobIgn = And (Not(Knows "b" (Var "bs"))) (Not(Knows "b" (Not (Var "bs"))))

{-problem2 : Exprime le problème 2 dans sa totalité-}
problem2 :: EpiFormula
problem2 = 
    And (And aliceIgn bobIgn)
        (After  fatherAnn 
                (After (And aliceIgn bobIgn) (And (Not aliceIgn) (Not bobIgn)))
        )

testProblem2 :: Bool
testProblem2 = epiSat s0 problem2