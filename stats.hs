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

data DaytumFieldStats a = DaytumFieldStats { minimum :: a,
                                   maximum :: a,
                                   count   :: Int
                                 } deriving Show

fieldStats :: Ord a =>  DaytumField a -> [DaytumRecord] -> DaytumFieldStats a
fieldStats f xs = DaytumFieldStats { minimum = DL.minimum xs', maximum = DL.maximum xs', count = length xs' }
  where
    xs' = map f xs

data DaytumAmountStats a = DaytumAmountStats { least   :: a,
                                               most    :: a,
                                               average :: a
                                             } deriving Show

amountStats :: [DaytumRecord] -> DaytumAmountStats Double
amountStats xs = DaytumAmountStats { least = minimum stats, most = maximum stats, average = avg }
  where
    stats = fieldStats amount xs
    xs' = map amount xs
    avg = sum xs' / fromIntegral (length xs)

data DaytumDateStats = DaytumDateStats { first :: DateTime,
                                         last  :: DateTime,
                                         averageDaysBetween :: Double
                                       } deriving Show

dateStats :: [DaytumRecord] -> DaytumDateStats
dateStats xs = DaytumDateStats { first = minimum stats, last = maximum stats, averageDaysBetween = 0.0 }
  where stats = fieldStats date xs
