module Daytum.Filter
  (
    filterFieldByEq,
    filterFieldByContains,
    filterFieldByRange
  )
where

import Daytum

type DaytumFilter = [DaytumRecord] -> [DaytumRecord]

filterField :: DaytumField a -> (a -> Bool) -> DaytumFilter
filterField field predicate records = filter decider records
  where decider record = predicate $ field record

filterFieldByEq :: Eq a =>  DaytumField a -> a -> DaytumFilter
filterFieldByEq field value records = filterField field predicate records
  where predicate x = x == value

filterFieldByContains :: Eq a =>  DaytumField [a] -> a -> DaytumFilter
filterFieldByContains field value records = filterField field predicate records
  where predicate x = value `elem` x

filterFieldByRange :: Ord a => DaytumField a -> a -> a -> DaytumFilter
filterFieldByRange f minVal maxVal xs = filter decider xs
  where
    decider record = let value = f record in minVal <= value && value <= maxVal

