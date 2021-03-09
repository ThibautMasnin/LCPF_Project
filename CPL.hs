
module CPL where

data Formula = T
    | F
    | Var String
    | Not   Formula
    | And   Formula Formula
    | Or    Formula Formula
    | Imp   Formula Formula
    | Eqv   Formula Formula
    deriving (Eq, Show)

type World = [String]
genAllWorlds :: [String] -> [World]
genAllWorlds [] = []
genAllWorlds (x:xs) = [x] : map (x :) (genAllWorlds xs) ++ genAllWorlds xs