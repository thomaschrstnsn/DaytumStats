{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import System.Environment (getArgs)

import Safe
import CSV (parseCSV)

main = do
  ~[fname] <- getArgs
  putStrLn $ "parsing: " ++ show fname
  res <- parseCSV fname
  case res of
    Left err -> print err
    Right xs -> print $ map fromList $ drop 1 xs

type Activity = String
data DaytumRecord = Daytum { name       :: String
                           , date       :: String
                           , amount     :: Double
                           , activities :: [Activity]
                           } deriving Show

-- | Creates a DaytumRecord from list of strings
fromList :: [String] -> DaytumRecord
fromList [ns, ds, vs, as] = Daytum {name=ns, date=ds, amount=amount, activities=acts}
  where
    amount = (readNote "amount" vs)::Double
    acts   = activitiesFromList as
fromList _ = error "could not parse daytum record"

activitiesFromList :: String -> [Activity]
activitiesFromList xs = afl xs []
  where
    afl :: String -> String -> [String]
    afl [] w = [w]
    afl xs w = case xs of 
      ' ':xs -> afl xs w
      ';':xs -> w:(afl xs [])
      x:xs   -> afl xs (w++[x])


