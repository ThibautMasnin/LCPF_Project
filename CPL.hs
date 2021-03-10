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

type World = [String]

supprDoublons :: [String] -> [String] -> [String]
supprDoublons [] sortie = sortie
supprDoublons (x:entre) sortie
    | x `elem` sortie = supprDoublons entre sortie
    | otherwise = supprDoublons entre (x:sortie)

getVars :: Formula -> [String]
getVars phi = supprDoublons (getVarsAux phi) []

getVarsAux :: Formula -> [String]
getVarsAux T = []
getVarsAux F = []
getVarsAux (Var s) = [s]
getVarsAux (Not phi) = getVarsAux phi
getVarsAux (And phi psi) = getVarsAux phi ++ getVarsAux psi
getVarsAux (Or phi psi) = getVarsAux phi ++ getVarsAux psi
getVarsAux (Imp phi psi) = getVarsAux phi ++ getVarsAux psi
getVarsAux (Eqv phi psi) = getVarsAux phi ++ getVarsAux psi


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

findWorldsAux :: Formula -> [World]  -> [World]
findWorldsAux _ []  = []
findWorldsAux  phi (w:allW)
    | sat w phi = w : findWorldsAux phi allW
    | otherwise = findWorldsAux phi allW 