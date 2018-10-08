module Rat where

import Data.List (intercalate, (\\))
import Sgd (sgd)

-- | Infinite list of rationals, ordered by sum of terms (p/q)
allRats :: [(Integer, Integer)]
allRats = gen 2 1 1
  where gen total p q =
          (p, q) : (if q == 1
                    then gen (total + 1) 1 total
                    else gen total (p + 1) (q - 1))

-- | String representation of a pair of integers as a rational number
showRat :: (Integer, Integer) -> String
showRat (p, q) = show p ++ "/" ++ show q

-- | Can this rational be shortened?
canBeShortened :: (Integer, Integer) -> Bool
canBeShortened (p, q) =
  sgd p q > 1

-- | Print a bunch of rational numbers
printRats :: IO ()
printRats = do
  putStrLn
    $ intercalate ", "
    $ map showRat
    $ take 200
    $ filter (not . canBeShortened)
    $ allRats



-- | The nth rational
nthRat :: Integer -> (Integer, Integer) -> (Integer, Integer)
nthRat 0 x = x
nthRat n (p, q) | q == 1       = nthRat (n - 1) (1, p + q)
                | otherwise    = nthRat (n - 1) (p + 1, q - 1)

showcase =
  putStrLn
  $ intercalate ", "
  $ map showRat
  $ take 200
  $ map (\n -> nthRat n (0, 1)) [0..20]


-- | More like the mathematical definition - all the sets (in order) where for a set 'n', p+q = n
nthSet n = [(p, q) | p <- [-n..n], q <- [-n..n] \\ [0], (abs (p + q) == n) && sgd p q == 1]
allSets = concatMap nthSet [0..]
printNicely = putStrLn $ (intercalate ", " . map showRat . take 50) allSets
