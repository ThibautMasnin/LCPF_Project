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