{-# LANGUAGE ScopedTypeVariables #-}
module Daytum
  (
    Category,
    DaytumRecord(..),
    DaytumField,
    fieldExtract,
    uniqueFields,
    uniqueCategories,
    filterByField,
    daytumsFromCsvFile,
    daytumFromCsvLine
  )
where

import Data.DateTime
import System.Locale
import Safe
import Data.CSV (parseCSV)
import qualified Data.List as DL

type Category = String
data DaytumRecord = Daytum { name       :: String
                           , date       :: DateTime
                           , amount     :: Double
                           , categories :: [Category]
                           } deriving (Eq, Show)

type DaytumField a = DaytumRecord -> a

fieldExtract :: DaytumField a -> [DaytumRecord] -> [a]
fieldExtract f xs = map f xs

uniqueFields :: Eq a =>  DaytumField a -> [DaytumRecord] -> [a]
uniqueFields f xs = DL.nub $ map f xs

uniqueCategories :: [DaytumRecord] -> [Category]
uniqueCategories xs = DL.nub $ concat $ uniqueFields categories xs

filterByField :: Eq a =>  DaytumField a -> a -> [DaytumRecord] -> [DaytumRecord]
filterByField f value xs = filter decider xs
  where decider record = value == f record

-- | Parses a csv file as a list of daytum records
daytumsFromCsvFile :: String -> IO [DaytumRecord]
daytumsFromCsvFile fname = do
  csvParse <- parseCSV fname
  return (case csvParse of
    Left err -> error $ show err
    Right xs -> fmap daytumFromCsvLine $ drop 1 xs)

-- | Creates a DaytumRecord from list of strings
daytumFromCsvLine :: [String] -> DaytumRecord
daytumFromCsvLine [ns, ds, vs, as] = Daytum {name=ns, date=date, amount=amount, categories=cats}
  where
    date   = daytumDateParse ds
    amount = (readNote "amount" vs)::Double
    cats   = categoriesFromList as
daytumFromCsvLine _ = error "could not parse daytum record"

-- | Example: Mon Jan 25 17:05:18 UTC 2010
daytumDateParse :: String -> DateTime
daytumDateParse x = case pd x of Just x -> x
                                 otherwise -> error $ "failed to parse string " ++ x ++ " as datetime"
  where pd = parseDateTime $ dateTimeFmt defaultTimeLocale

categoriesFromList :: String -> [Category]
categoriesFromList xs = afl xs []
  where
    afl :: String -> String -> [String]
    afl [] w = [w]
    afl xs w = case xs of 
      ' ':xs -> afl xs w
      ';':xs -> w:(afl xs [])
      x:xs   -> afl xs (w++[x])

