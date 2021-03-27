{-# OPTIONS_GHC -Wall #-}

module EL where

type Prop = [Char]
type Agent = [Char]
type World = Int
type EpiState = (Prop -> [World], Agent -> World -> [World], World)