{-# LANGUAGE ScopedTypeVariables #-}
module Daytum
  (
    DaytumRecord(..),
    Activity,
    daytumsFromCsvFile,
    daytumFromCsvLine
  )
where

import Data.DateTime
import System.Locale
import Safe
import CSV (parseCSV)

type Activity = String
data DaytumRecord = Daytum { name       :: String
                           , date       :: DateTime
                           , amount     :: Double
                           , activities :: [Activity]
                           } deriving Show

-- | Parses a csv file as a list of daytum records
daytumsFromCsvFile :: String -> IO [DaytumRecord]
daytumsFromCsvFile fname = do
  csvParse <- parseCSV fname
  return (case csvParse of
    Left err -> error $ show err
    Right xs -> fmap daytumFromCsvLine $ drop 1 xs)

-- | Creates a DaytumRecord from list of strings
daytumFromCsvLine :: [String] -> DaytumRecord
daytumFromCsvLine [ns, ds, vs, as] = Daytum {name=ns, date=date, amount=amount, activities=acts}
  where
    date   = daytumDateParse ds
    amount = (readNote "amount" vs)::Double
    acts   = activitiesFromList as
daytumFromCsvLine _ = error "could not parse daytum record"

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

