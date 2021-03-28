{-# OPTIONS_GHC -Wall #-} 

module Cha5 where
import CPL

{-door1 : "Choisis bien ta cellule ça’a a de l’importance !"-}
door1 :: Formula
door1 = Or (And (Var "p1") (Var "t2")) (And (Var "p2") (Var "t1"))

{-door2 : "Tu ferais mieux de choisir l’autre cellule !"-}
door2 :: Formula
door2 = Not (Var "t1")

{- constraint : décrit le fait qu’il ne peut pas y avoir un tigre et 
une princesse (en même temps) dans chaque cellule-}
constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

{-reglement :  l’affiche collé sur la cellule 1 dit la vérité quand il y a une princesse dans cette
cellule et ment quand c’est un tigre. Pour la cellule 2 c’est exactement le contraire
; quand il y a une princesse l’affiche ment et quand c’est un tigre l’affiche dit la vérité-}
reglement :: Formula
reglement = And (And (Imp (Var "p1") door1) (Imp (Var "t1") (Not door1))) (And (Imp (Var "t2") door2) (Imp (Var "p2") (Not door2)))

{-challenge5 : fait la conjonction de toutes les formules de la première épreuve-}
challenge5 :: Formula 
challenge5 = And reglement constraint