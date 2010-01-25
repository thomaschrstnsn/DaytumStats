{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import CSV (parseCSV)
import System.Environment (getArgs)

main = do
  ~[fname] <- getArgs
  putStrLn $ "parsing: " ++ show fname
  res <- parseCSV fname
  case res of 
    Left err -> print err
    Right xs -> print $ fromList $ xs !! 1

type Activity = String
data DaytumRecord = Daytum String String Double [Activity]
                    deriving Show

-- | Creates a DaytumRecord from list of strings
fromList :: [String] -> [DaytumRecord]
fromList [ns :: String, ds :: String , vs :: String , as :: String] = 
  [Daytum name date value acts]
  where
    name   = ns
    date   = ds
    value  = (read vs)::Double
    acts   = activitiesFromList as
fromList _ = []

activitiesFromList :: String -> [Activity]
activitiesFromList xs = afl xs []
  where
    afl :: String -> String -> [String]
    afl [] w = [w]
    afl xs w = case xs of 
      ' ':xs -> afl xs w
      ';':xs -> w:(afl xs [])
      x:xs   -> afl xs (w++[x])
