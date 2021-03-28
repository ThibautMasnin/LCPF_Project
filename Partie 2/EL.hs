{-# OPTIONS_GHC -Wall #-}

module EL where

{-Prop : chaîne de caractères-}
type Prop = String

{-Agent : chaîne de caractères-}
type Agent = String

{-World : entier-}
type World = Int

{-EpiState : tuple-}
type EpiState = (Prop -> [World], Agent -> World -> [World], World)

{-data EpiFormula : Exprime les épreuves sous la forme d’une formule logique-}
data EpiFormula = T
    | F
    | Var Prop
    | Not EpiFormula
    | And EpiFormula EpiFormula
    | Or EpiFormula EpiFormula
    | Imp EpiFormula EpiFormula
    | Eqv EpiFormula EpiFormula
    | Knows Agent EpiFormula
    | After EpiFormula EpiFormula
    deriving (Eq, Show)

{-epiSat : prend un état épistémique s et une formule phi en arguments et renvoie True si s
satisfait phi, et False sinon-}
epiSat :: EpiState -> EpiFormula -> Bool
epiSat _ T = True
epiSat _ F = False
epiSat (interp, _, w) (Var p) = w `elem` interp p
epiSat s (Not phi) = not (epiSat s phi)
epiSat s (And phi psi) = epiSat s phi && epiSat s psi
epiSat s (Or phi psi) = epiSat s phi || epiSat s psi
epiSat s (Imp phi psi) = not(epiSat s phi) || epiSat s psi
epiSat s (Eqv phi psi) = epiSat s (Imp phi psi) && epiSat s (Imp psi phi)
epiSat (interp, indis, w) (Knows a phi) = all (\x->(epiSat (interp, indis, x) phi)) (indis a w)
epiSat (interp, indis, w) (After phi psi) = (epiSat (interp, indis, w) phi) && (epiSat(update(interp, indis, w) phi) psi)

interpTest :: Prop -> [World]
interpTest "as" = [10, 11]
interpTest "bs" = [01, 11]
interpTest _    = [] 

indisTest :: Agent -> World -> [World]
indisTest "a" 00 = [00, 10]
indisTest "b" 00 = [00, 01]
indisTest "a" 01 = [01, 11]
indisTest "b" 01 = [01, 00]
indisTest "a" 10 = [10, 00]
indisTest "b" 10 = [10, 11]
indisTest "a" 11 = [11, 01]
indisTest "b" 11 = [11, 10]
indisTest _ _ = []

testEpiSat :: [Bool]
testEpiSat = [ epiSat (interpTest, indisTest, 01) (And (And (And (Not(Knows "a" (Var "as"))) (Not(Knows "a" (Not (Var "as"))))) (And (Not(Knows "b" (Var "bs"))) (Not(Knows "b" (Not (Var "bs")))))) (After (Or (Var "as") (Or (Var "bs") (And (Var "as") (Var "bs")))) (And (And (And (Not(Knows "a" (Var "as"))) (Not(Knows "a" (Not (Var "as"))))) (Not (And (Not(Knows "b" (Var "bs"))) (Not(Knows "b" (Not (Var "bs"))))))) (After (Not (And (Not(Knows "b" (Var "bs"))) (Not(Knows "b" (Not (Var "bs")))))) (Not (And (Not(Knows "a" (Var "as"))) (Not(Knows "a" (Not (Var "as")))))))))) == True ]

{-update : prend un état épistémique s et une formule phi et
renvoie un nouvel état épistémique correspondant à la mise à jour de s par phi-}
update :: EpiState -> EpiFormula -> EpiState
update (interp, indis, w) phi =
    let 
        newInterp p = filter (\x-> (epiSat (interp, indis, x) phi)) (interp p)
        newIndis a newW = filter (\x-> (epiSat (interp, indis, x) phi)) (indis a newW)
    in (newInterp, newIndis, w)

testUpdate :: [Bool]
testUpdate = [  ] 
    
{-test : fonction qui reçoit les résultats d’un test et qui retourne vrai
si tous les résultats du test sont vrai et faux sinon.-}
test :: [Bool] -> Bool 
test [] = True
test x = all (==True) x

{- testAll : Fonction qui retourne "Success!" si tous les résultats des tests de toutes 
les fonctions sont vrais, sinon "Fail!"-}
testAll :: [Char]
testAll 
    | test(testEpiSat) && test(testUpdate) = "Success!"
    | otherwise = "Fail!"