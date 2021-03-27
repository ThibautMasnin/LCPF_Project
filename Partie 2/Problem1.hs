{-# OPTIONS_GHC -Wall #-}

import EL

interp :: Prop -> [World]
interp "as" = [10, 11]
interp "bs" = [01, 11]
interp _    = [] 

indis :: Agent -> World -> [World]
indis "a" 00 = [00, 10]
indis "b" 00 = [00, 01]
indis "a" 01 = [01, 11]
indis "b" 01 = [01, 00]
indis "a" 10 = [10, 00]
indis "b" 10 = [10, 11]
indis "a" 11 = [11, 01]
indis "b" 11 = [11, 10]
indis _ _ = []

s0 :: EpiState
s0 = (interp, indis, 01)


