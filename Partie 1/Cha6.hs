
{-# OPTIONS_GHC -Wall #-} 

import CPL

door1 :: Formula
door1 = Var "t1"

door2 :: Formula
door2 = Var "p2"

door3 :: Formula
door3 = Var "t2"

constraint :: Formula
constraint = And (And (Eqv (Var "p1") (Not (Var "t1"))) (Imp (Var "p1") (And (Var "t2") (Var "t3")))) (And (And (Eqv (Var "p2") (Not (Var "t2"))) (Imp (Var "p2") (And (Var "t1") (Var "t3")))) (And (Eqv (Var "p3") (Not (Var "t3"))) (Imp (Var "p3") (And (Var "t1") (Var "t2")))))

reglement :: Formula
reglement = Or (And door1 (And (Not door2) (Not door3))) (Or (And door2 (And (Not door1) (Not door3))) (And door3 (And (Not door1) (Not door2))))

challenge6 :: Formula 
challenge6 = And reglement constraint