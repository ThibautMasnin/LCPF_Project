
{-# OPTIONS_GHC -Wall #-}

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

getVars :: Formula -> [String]
getVars T = []
getVars F = []
getVars (Var s) = [s]
getVars (Not phi) = getVars phi
getVars (And phi psi) = getVars phi ++ getVars psi
getVars (Or phi psi) = getVars phi ++ getVars psi
getVars (Imp phi psi) = getVars phi ++ getVars psi
getVars (Eqv phi psi) = getVars phi ++ getVars psi

type World = [String]
genAllWorlds :: [String] -> [World]
genAllWorlds [] = []
genAllWorlds (x:xs) = [x] : map (x :) (genAllWorlds xs) ++ genAllWorlds xs

sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat w (Var s) = s `elem` w
sat w (Not phi) = not (sat w phi)
sat w (And phi psi) = sat w phi && sat w psi
sat w (Or phi psi) = sat w phi || sat w psi
sat w (Imp phi psi) = not(sat w phi) || sat w psi
sat w (Eqv phi psi) = sat w (Imp phi psi) && sat w (Imp psi phi)


findWorlds :: Formula -> [World]
findWorlds phi = findWorldsAux phi (genAllWorlds (getVars phi))

findWorldsAux :: Formula -> [World] -> [World]
findWorldsAux phi (w:allW)
    | allW == [] && sat w phi = [w]
    | allW == [] = []
    | sat w phi = [w] ++ findWorldsAux phi allW
    | otherwise = [] ++ findWorldsAux phi allW 