module Main
where

import CSV (parseCSV)
import System.Environment (getArgs)

main = do
  ~[fname] <- getArgs
  putStrLn $ "parsing: " ++ show fname
  res <- parseCSV fname
  case res of 
    Left err -> print err
    Right xs -> putStrLn $ "result: " ++ show xs

