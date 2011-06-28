{-# LANGUAGE DisambiguateRecordFields #-}
module Daytum.Stats
  (
    DaytumFieldStats, fieldStats,
    DaytumAmountStats, amountStats,
    DaytumDateStats, dateStats
  )
where

import Prelude hiding (minimum, maximum)
import Daytum
import qualified Data.List as DL
import Data.DateTime
import Data.Time.Clock

data DaytumFieldStats a =
  DaytumFieldStats
    { minimum :: a,
      maximum :: a
    } deriving Show

fieldStats :: Ord a =>  DaytumField a -> [DaytumRecord] -> DaytumFieldStats a
fieldStats f xs = let xs' = map f xs in
                  DaytumFieldStats { minimum = DL.minimum xs', maximum = DL.maximum xs' }

data DaytumAmountStats a =
  DaytumAmountStats
    { least   :: a,
      most    :: a,
      average :: a
    } deriving Show

amountStats :: [DaytumRecord] -> DaytumAmountStats Double
amountStats xs = DaytumAmountStats { least = minimum stats, most = maximum stats, average = avg }
  where
    stats = fieldStats amount xs
    xs' = map amount xs
    avg = DL.sum xs' / fromIntegral (length xs)

data DaytumDateStats =
  DaytumDateStats
    { first              :: DateTime,
      last               :: DateTime,
      averageDaysBetween :: Double
    } deriving Show

dateStats :: [DaytumRecord] -> DaytumDateStats
dateStats xs = DaytumDateStats { first = minimum stats, last = maximum stats, averageDaysBetween = avg }
  where
    stats = fieldStats date xs
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
