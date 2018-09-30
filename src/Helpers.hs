module Helpers where

-- | Hjälpfunktion för att skriva ut ett rutnät av heltal
printIntTable :: Integer -> Integer -> (Integer -> Integer -> String) -> IO ()
printIntTable rows columns f =
  do putStr "\t"
     mapM_ (\column -> putStr (show column ++ "\t")) [1..columns]
     putStrLn ""
     mapM_ (\row -> putStrLn (show row ++ printIntRow row)) [1..rows]
     putStrLn ""
  where printIntRow row = concatMap (\column -> "\t" ++ f column row) [1..columns]

showCheckmark :: Bool -> String
showCheckmark True = "•"
showCheckmark False = " "
