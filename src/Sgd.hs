module Sgd where

import Helpers

-- | Största gemensamma delare (Euklides algoritm)
sgd :: Integer -> Integer -> Integer
sgd a b =
  let m = div a b
      r = mod a b
  in case r of
       0 -> b
       x -> sgd b x

-- | Är två tal relativt prima?
relativtPrima :: Integer -> Integer -> Bool
relativtPrima a b =
  sgd a b == 1

-- | Resultat
gridSize = 40
printSgd = printIntTable gridSize gridSize (\a b -> show (sgd a b))
printRelativePrime = printIntTable gridSize gridSize (\a b -> showCheckmark (relativtPrima a b))

-- | Multiplikationstabell för kongruensklasser av heltal
n = 11
mods = printIntTable n n (\a b -> show (mod (a * b) n))
