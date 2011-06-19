{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import System.Environment (getArgs)
import Data.DateTime
import System.Locale
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
                           , date       :: DateTime
                           , amount     :: Double
                           , activities :: [Activity]
                           } deriving Show

-- | Creates a DaytumRecord from list of strings
fromList :: [String] -> DaytumRecord
fromList [ns, ds, vs, as] = Daytum {name=ns, date=date, amount=amount, activities=acts}
  where
    date   = daytumDateParse ds
    amount = (readNote "amount" vs)::Double
    acts   = activitiesFromList as
fromList _ = error "could not parse daytum record"

-- | Example: Mon Jan 25 17:05:18 UTC 2010
daytumDateParse :: String -> DateTime
daytumDateParse x = case pd x of Just x -> x
                                 otherwise -> error $ "failed to parse string " ++ x ++ " as datetime"
  where pd = parseDateTime $ "%a %b %e %H:%M:%S %Z %Y" -- should be: dateTimeFmt from defaultTimeLocale

activitiesFromList :: String -> [Activity]
activitiesFromList xs = afl xs []
  where
    afl :: String -> String -> [String]
    afl [] w = [w]
    afl xs w = case xs of 
      ' ':xs -> afl xs w
      ';':xs -> w:(afl xs [])
      x:xs   -> afl xs (w++[x])


