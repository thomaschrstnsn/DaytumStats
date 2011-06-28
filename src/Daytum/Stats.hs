{-# LANGUAGE DisambiguateRecordFields #-}
module Daytum.Stats
  (
    OrderedStats, orderedStats,
    AmountStats, amountStats,
    DateStats, dateStats
  )
where

import Prelude hiding (minimum, maximum)
import Daytum
import qualified Data.List as DL
import Data.DateTime
import Data.Time.Clock

data OrderedStats a =
  OrderedStats
    { minimum :: a,
      maximum :: a
    } deriving Show

orderedStats :: Ord a =>  DaytumField a -> [DaytumRecord] -> OrderedStats a
orderedStats f xs = let xs' = map f xs in
                  OrderedStats { minimum = DL.minimum xs', maximum = DL.maximum xs' }

data AmountStats a =
  AmountStats
    { least   :: a,
      most    :: a,
      average :: a
    } deriving Show

amountStats :: [DaytumRecord] -> AmountStats Double
amountStats xs = AmountStats { least = minimum stats, most = maximum stats, average = avg }
  where
    stats = orderedStats amount xs
    xs' = map amount xs
    avg = DL.sum xs' / fromIntegral (length xs)

data DateStats =
  DateStats
    { first              :: DateTime,
      last               :: DateTime,
      averageDaysBetween :: Double
    } deriving Show

dateStats :: [DaytumRecord] -> DateStats
dateStats xs = DateStats { first = minimum stats, last = maximum stats, averageDaysBetween = avg }
  where
    stats = orderedStats date xs
    xs'   = map date xs
    deltadays = map toDays $ dateDiffs xs'
    avg   = DL.sum deltadays / fromIntegral (length xs)
    min   = DL.minimum xs'
    max   = DL.maximum xs'

dateDiffs :: [DateTime] -> [NominalDiffTime]
dateDiffs ds = map dd $ zip ds' $ tail ds'
  where
    ds' = DL.sort ds
    dd (x,y) = diffUTCTime y x

toDays :: Fractional a =>  NominalDiffTime -> a
toDays d = realToFrac d / secondsPerDay
  where
    secondsPerDay = 24 * 60 * 60
