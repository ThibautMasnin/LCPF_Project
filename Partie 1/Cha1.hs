{-# OPTIONS_GHC -Wall #-}

module Cha1 where
import CPL

{-door1 : "Il y a une princesse dans cette cellule et un tigre dans l’autre"-}
door1 :: Formula
door1 = And (Var "p1") (Var "t2")

{-door2 : "Il y a une princesse dans une cellule et il y a un tigre dans une cellule"-}
door2 :: Formula
door2 = And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

{- constraint : décrit le fait qu’il ne peut pas y avoir un tigre et 
une princesse (en même temps) dans chaque cellule-}
constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

{-reglement :  "Une des affiches dit la vérité et l’autre ment"-}
reglement :: Formula
reglement = Or (And door1 (Not door2)) (And (Not door1) door2)

{-challenge1 : fait la conjonction de toutes les formules de la première épreuve-}
challenge1 :: Formula 
challenge1 = And reglement constraint