{-# LANGUAGE DisambiguateRecordFields #-}
module Daytum.Stats
  (
    OrderedStats, orderedStats,
    NumStats, amountStats,
    DateStats, dateStats
  )
where

import Prelude hiding (minimum, maximum)
import Daytum
import qualified Data.List as DL
import qualified Data.List.Statistics as DLS
import Data.DateTime
import Data.Time.Clock

data OrderedStats a =
  OrderedStats
    { minimum :: a,
      maximum :: a
    } deriving Show

orderedStats :: Ord a =>  [a] -> OrderedStats a
orderedStats xs = OrderedStats { minimum = DL.minimum xs, maximum = DL.maximum xs }

data NumStats a =
  NumStats
    { order  :: OrderedStats a,
      mean   :: a,
      median :: a,
      range  :: a
    } deriving Show

numStats :: (Fractional a, Ord a) =>  [a] -> NumStats a
numStats xs = NumStats { order = oStats, mean = mean, median = median, range = range }
  where
    oStats = orderedStats xs
    mean   = DLS.mean xs
    median = DLS.median xs
    range  = maximum oStats - minimum oStats

amountStats :: [DaytumRecord] -> NumStats Double
amountStats xs = numStats $ fieldExtract amount xs

data DateStats =
  DateStats
    { orderDays   :: OrderedStats DateTime,
      daysBetween :: NumStats Double
    } deriving Show

dateStats :: [DaytumRecord] -> DateStats
dateStats xs = DateStats { orderDays = oStats, daysBetween = dStats }
  where
    xs'    = fieldExtract date xs
    oStats = orderedStats xs'
    deltadays  = map toDays $ dateDiffs xs'
    dStats = numStats deltadays

dateDiffs :: [DateTime] -> [NominalDiffTime]
dateDiffs ds = map dd $ zip ds' $ tail ds'
  where
    ds' = DL.sort ds
    dd (x,y) = diffUTCTime y x

toDays :: Fractional a =>  NominalDiffTime -> a
toDays d = realToFrac d / secondsPerDay
  where
    secondsPerDay = 24 * 60 * 60
