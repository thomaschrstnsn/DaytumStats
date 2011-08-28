module Main
where

import System.Environment (getArgs)
import Daytum
import Daytum.Stats
import Daytum.Filter

main = do
  ~[fname] <- getArgs
  putStrLn $ "parsing: " ++ show fname
  records <- daytumsFromCsvFile fname
  putStrLn $ show $ uniqueCategories records

  let udgifter = filterFieldByContains categories "AlleUdgifter" records
  let dyre = filterFieldByRange amount 100 100000 udgifter
  putStrLn $ show $ length $ udgifter
  putStrLn $ show $ length $ dyre

  putStrLn $ show $ amountStats udgifter
