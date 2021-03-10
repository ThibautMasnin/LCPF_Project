
{-# OPTIONS_GHC -Wall #-} 
 
import CPL 
 
 
door1 :: Formula 
door1 = Or (Var "p1") (Or (Var "p2") (And (Var "p1") (Var "p2"))) 
 
 
door2 :: Formula 
door2 = Var "t1" 
 
 
constraint :: Formula 
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2"))) 
 
 
reglement :: Formula 
reglement = Or (And door1 door2) (And (Not door1) (Not door2)) 
 
 
challenge2 :: Formula 
challenge2 = And constraint reglement