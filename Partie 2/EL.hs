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

{-update : prend un état épistémique s et une formule phi et
renvoie un nouvel état épistémique correspondant à la mise à jour de s par phi-}
update :: EpiState -> EpiFormula -> EpiState
update (interp, indis, w) phi =
    let 
        newInterp p = filter (\x-> (epiSat (interp, indis, x) phi)) (interp p)
        newIndis a newW = filter (\x-> (epiSat (interp, indis, x) phi)) (indis a newW)
    in (newInterp, newIndis, w)