
{-# OPTIONS_GHC -Wall #-}

import CPL

door1 :: Formula
door1 = And (Var "p1") (Var "t2")

door2 :: Formula
door2 = And (Or (Var "p1") (Var "p2")) (Or (Var "t1") (Var "t2"))

constraint :: Formula
constraint = Or (And (Var "p1") (Var "t2")) (And (Var "p2") (Var "t1"))

reglement :: Formula
reglement = Or door1 door2

challenge1 :: Formula 
challenge1 = And reglement constraint