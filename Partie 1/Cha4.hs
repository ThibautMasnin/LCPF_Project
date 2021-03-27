
{-# OPTIONS_GHC -Wall #-}

import CPL


door1 :: Formula
door1 = Or (And (Var "p1") (Var "p2")) (And (Var "t1") (Var "t2"))


door2 :: Formula
door2 = Var "p1"


constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))


reglement :: Formula
reglement = And (And (Imp (Var "p1") door1) (Imp (Var "t1") (Not door1))) (And (Imp (Var "t2") door2) (Imp (Var "p2") (Not door2)))

challenge4 :: Formula
challenge4 = And constraint reglement