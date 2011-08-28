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
  putStrLn $ show $ uniqueFields name records
  putStrLn $ "showing entries with name: Spinning"
  let spinning = filterFieldByEq name "Spinning" records
  putStrLn $ show $ dateStats spinning

  let actRunning = "Running"
  let actRunningNoAct = "Running (No Activity)"
  let pureRunning = filterFieldByEq name actRunning records
  let nonPureRunning = filterFieldByEq name actRunningNoAct records
  let running = pureRunning ++ nonPureRunning
  putStrLn $ "showing entries with name: Running and friends"
  putStrLn $ show $ amountStats running
  putStrLn $ show $ dateStats running
  putStrLn $ "Non-pure running records:"
  putStrLn $ show $ length nonPureRunning 


