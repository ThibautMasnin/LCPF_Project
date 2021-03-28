{-# OPTIONS_GHC -Wall #-} 

module Cha6 where
import CPL

{-door1 : "Il y a un tigre ici"-}
door1 :: Formula
door1 = Var "t1"

{-door2 : "Cette cellule contient une princesse"-}
door2 :: Formula
door2 = Var "p2"

{-door3 : "Il y a un tigre dans la cellule 2"-}
door3 :: Formula
door3 = Var "t2"

{- constraint : décrit le fait qu’il ne peut pas y avoir un tigre et 
une princesse (en même temps) dans chaque cellule, une seule cellule renfermait une princesse
et qu’il avait fait mettre un tigre dans chacune des deux autres-}
constraint :: Formula
constraint = And (And (Eqv (Var "p1") (Not (Var "t1"))) (Imp (Var "p1") (And (Var "t2") (Var "t3")))) (And (And (Eqv (Var "p2") (Not (Var "t2"))) (Imp (Var "p2") (And (Var "t1") (Var "t3")))) (And (Eqv (Var "p3") (Not (Var "t3"))) (Imp (Var "p3") (And (Var "t1") (Var "t2")))))

{-reglement :  "une seule de trois affiches était sincère"-}
reglement :: Formula
reglement = Or (And door1 (And (Not door2) (Not door3))) (Or (And door2 (And (Not door1) (Not door3))) (And door3 (And (Not door1) (Not door2))))

{-challenge6 : fait la conjonction de toutes les formules de la première épreuve-}
challenge6 :: Formula 
challenge6 = And reglement constraint