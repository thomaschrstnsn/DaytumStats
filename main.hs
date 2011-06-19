module Main
where

import Prelude hiding (minimum, maximum)
import System.Environment (getArgs)
import qualified Data.List as DL
import Daytum

main = do
  ~[fname] <- getArgs
  putStrLn $ "parsing: " ++ show fname
  records <- daytumsFromCsvFile fname
  putStrLn $ show $ uniqueFields name records
  putStrLn $ "showing entries with name: Spinning"
  let spinning = filterByField name "Spinning" records
  putStrLn $ show $ doStats date spinning

  let running = filterByField name "Running" records ++ filterByField name "Running (No Activity)" records
  putStrLn $ "showing entries with name: Running and friends"
  putStrLn $ show $ amountStats running

type DaytumField a = DaytumRecord -> a

uniqueFields :: Eq a =>  DaytumField a -> [DaytumRecord] -> [a]
uniqueFields f xs = DL.nub $ map f xs

filterByField :: Eq a =>  DaytumField a -> a -> [DaytumRecord] -> [DaytumRecord]
filterByField f value xs = filter decider xs
  where decider record = value == f record

data DaytumStats a = DaytumStats { minimum :: a,
                                   maximum :: a,
                                   count   :: Int
                                 } deriving Show

doStats :: Ord a =>  DaytumField a -> [DaytumRecord] -> DaytumStats a
doStats f xs = DaytumStats { minimum = DL.minimum xs', maximum = DL.maximum xs', count = length xs' }
  where
    xs' = map f xs

data DaytumAmountStats a = DaytumAmountStats { least   :: a,
                                               most    :: a,
                                               average :: a
                                             } deriving Show

amountStats :: [DaytumRecord] -> DaytumAmountStats Double
amountStats xs = DaytumAmountStats { least = minimum stats, most = maximum stats, average = avg }
  where
    stats = doStats amount xs
    xs' = map amount xs
    avg = sum xs' / fromIntegral (length xs')

-- data DaytumDateStats = DaytumDateStats { first :: DateTime
