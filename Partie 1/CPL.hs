{-# OPTIONS_GHC -Wall #-}

module CPL where

{-data Formula : Exprime les épreuves sous la forme d’une formule logique-}
data Formula = T
    | F
    | Var String
    | Not   Formula
    | And   Formula Formula
    | Or    Formula Formula
    | Imp   Formula Formula
    | Eqv   Formula Formula
    deriving (Eq, Show)

{-type world = Représente le type des mondes possibles-}
type World = [String]

{-getVars / supprDoublons / getVarsAux : Fonction auxiliaire qui “extrait” les noms de variables de la formule-}
getVars :: Formula -> [String]
getVars phi = supprDoublons (getVarsAux phi) []

supprDoublons :: [String] -> [String] -> [String]
supprDoublons [] sortie = sortie
supprDoublons (x:entre) sortie
    | x `elem` sortie = supprDoublons entre sortie
    | otherwise = supprDoublons entre (x:sortie)

getVarsAux :: Formula -> [String]
getVarsAux T = []
getVarsAux F = []
getVarsAux (Var s) = [s]
getVarsAux (Not phi) = getVarsAux phi
getVarsAux (And phi psi) = getVarsAux phi ++ getVarsAux psi
getVarsAux (Or phi psi) = getVarsAux phi ++ getVarsAux psi
getVarsAux (Imp phi psi) = getVarsAux phi ++ getVarsAux psi
getVarsAux (Eqv phi psi) = getVarsAux phi ++ getVarsAux psi

{-genAllworlds : pour une liste de noms de variables propositionnels,
génère la liste de tous les mondes possibles pour ces variables-}
genAllWorlds :: [String] -> [World]
genAllWorlds [] = []
genAllWorlds (x:xs) = [x] : map (x :) (genAllWorlds xs) ++ genAllWorlds xs

{-sat : pour un monde possible w et une formule phi passés en arguments, vérifie si w
satisfait phi-}
sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat w (Var s) = s `elem` w
sat w (Not phi) = not (sat w phi)
sat w (And phi psi) = sat w phi && sat w psi
sat w (Or phi psi) = sat w phi || sat w psi
sat w (Imp phi psi) = not(sat w phi) || sat w psi
sat w (Eqv phi psi) = sat w (Imp phi psi) && sat w (Imp psi phi)

{-findWorlds / findWorldsAux : pour une formule phi renvoie la liste de tous mondes possibles qui
satisfont phi.-}
findWorlds :: Formula -> [World]
findWorlds phi = findWorldsAux phi (genAllWorlds (getVars phi))

findWorldsAux :: Formula -> [World]  -> [World]
findWorldsAux _ []  = []
findWorldsAux  phi (w:allW)
    | sat w phi = w : findWorldsAux phi allW
    | otherwise = findWorldsAux phi allW 