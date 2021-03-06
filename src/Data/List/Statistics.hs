module Data.List.Statistics
  (
    mean, median, standardDeviation
  )
where

import qualified Data.List as DL

mean :: Fractional a =>  [a] -> a
mean xs = DL.sum xs / fromIntegral (length xs)

median :: (Fractional a, Ord a) =>  [a] -> a
median xs = if even len
              then avgMiddle
              else middleElem
  where
    len        = length xs
    xs'        = DL.sort xs

    halfLen    = len `div` 2
    middleElem = head $ drop halfLen xs'

    twoMiddle  = take 2 $ drop (halfLen - 1) xs'
    avgMiddle  = mean twoMiddle

standardDeviation :: Floating a => [a] -> a
standardDeviation xs = sqrt $ mean squareDiffs
  where
    meanValue   = mean xs
    squareDiffs = map (\x -> x*x) $ map (meanValue -) xs
