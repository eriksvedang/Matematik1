module Rat where

import Data.List (intercalate)
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
