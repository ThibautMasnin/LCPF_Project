
import CPL

door1 :: Formula
door1 = Var "t1"

door2 :: Formula
door2 = Var "p2"

door3 :: Formula
door3 = Var "t2"

constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

reglement :: Formula
reglement =

challenge6 :: Formula 
challenge6 = And reglement constraint