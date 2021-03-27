{-# OPTIONS_GHC -Wall #-}

module EL where

type Prop = String
type Agent = String
type World = Int
type EpiState = (Prop -> [World], Agent -> World -> [World], World)

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

update :: EpiState -> EpiFormula -> EpiState
update (interp, indis, w) phi =
    let 
        newInterp p = filter (\x-> (epiSat (interp, indis, x) phi)) (interp p)
        newIndis a newW = filter (\x-> (epiSat (interp, indis, x) phi)) (indis a newW)
    in (newInterp, newIndis, w)