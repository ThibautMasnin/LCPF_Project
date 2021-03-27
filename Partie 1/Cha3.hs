
{-# OPTIONS_GHC -Wall #-} 

import CPL

door1 :: Formula
door1 = Or (Var "t1") (Var "p2")

door2 :: Formula
door2 = Var "p1"

constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

reglement :: Formula
reglement = Or (And door1 door2) (And (Not door1) (Not door2))

challenge3 :: Formula 
challenge3 = And reglement constraint