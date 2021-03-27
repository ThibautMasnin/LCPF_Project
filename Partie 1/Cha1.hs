{-# OPTIONS_GHC -Wall #-}

import CPL

door1 :: Formula
door1 = And (Var "p1") (Var "t2")

door2 :: Formula
door2 = And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

reglement :: Formula
reglement = Or (And door1 (Not door2)) (And (Not door1) door2)

challenge1 :: Formula 
challenge1 = And reglement constraint