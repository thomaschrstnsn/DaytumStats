module Data.List.Statistics
  (
    average, median
  )
where

import qualified Data.List as DL

average :: Fractional a =>  [a] -> a
average xs = DL.sum xs / fromIntegral (length xs)

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
    avgMiddle  = average twoMiddle
