{-# OPTIONS_GHC -Wall #-} 

module Cha3 where
import CPL

{-door1 : "Il y a un tigre dans cette cellule ou il y a une princesse dans l’autre"-}
door1 :: Formula
door1 = Or (Var "t1") (Var "p2")

{-door2 : "Il y a une princesse dans l’autre cellule"-}
door2 :: Formula
door2 = Var "p1"

{- constraint : décrit le fait qu’il ne peut pas y avoir un tigre et 
une princesse (en même temps) dans chaque cellule-}
constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

{-reglement :  "Les affiches disent toutes les deux la vérité 
ou bien mentent toutes les deux"-}
reglement :: Formula
reglement = Or (And door1 door2) (And (Not door1) (Not door2))

{-challenge3 : fait la conjonction de toutes les formules de la première épreuve-}
challenge3 :: Formula 
challenge3 = And reglement constraint

testChallenge3 :: Bool
testChallenge3 = sat ["p1", "p2"] challenge3