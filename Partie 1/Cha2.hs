
{-# OPTIONS_GHC -Wall #-} 
 
 module Cha2 where
import CPL 
 
{-door1 : "Une au moins de deux cellules contient une princesse"-}
door1 :: Formula 
door1 = Or (Var "p1") (Or (Var "p2") (And (Var "p1") (Var "p2"))) 
 
{-door2 : "Il y a un tigre dans l’autre cellule"-} 
door2 :: Formula 
door2 = Var "t1" 
 
{- constraint : décrit le fait qu’il ne peut pas y avoir un tigre et 
une princesse (en même temps) dans chaque cellule-}
constraint :: Formula 
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2"))) 
 
 {-reglement :  "Les affiches disent toutes les deux la vérité 
 ou bien mentent toutes les deux"-}
reglement :: Formula 
reglement = Or (And door1 door2) (And (Not door1) (Not door2)) 
 
{-challenge2 : fait la conjonction de toutes les formules de la première épreuve-}
challenge2 :: Formula 
challenge2 = And constraint reglement