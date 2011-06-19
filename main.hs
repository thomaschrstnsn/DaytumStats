module Main
where

import System.Environment (getArgs)
import Daytum

main = do
  ~[fname] <- getArgs
  putStrLn $ "parsing: " ++ show fname
  records <- daytumsFromCsvFile fname
  putStrLn $ show $ records

