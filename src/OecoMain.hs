module Main
where

import System.Environment (getArgs)
import Daytum
import Daytum.Stats

main = do
  ~[fname] <- getArgs
  putStrLn $ "parsing: " ++ show fname
  records <- daytumsFromCsvFile fname
  putStrLn $ show $ uniqueFields name records
