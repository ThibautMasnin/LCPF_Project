{-# OPTIONS_GHC -Wall #-}

module EL where

type Prop = [Char]
type Agent = [Char]
type World = Int
type EpiState = (Prop -> [World], Agent -> World -> [World], World)

data EpiFormula = T
    | F
    | Var String
    | Not EpiFormula
    | And EpiFormula EpiFormula
    | Or EpiFormula EpiFormula
    | Imp EpiFormula EpiFormula
    | Eqv EpiFormula EpiFormula
    | Knows String EpiFormula
    | After EpiFormula EpiFormula
    deriving (Eq, Show)