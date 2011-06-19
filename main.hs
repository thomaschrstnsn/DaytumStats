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
  putStrLn $ "showing entries with name: Spinning"
  let spinning = filterByField name "Spinning" records
  putStrLn $ show $ fieldStats date spinning

  let actRunning = "Running"
  let actRunningNoAct = "Running (No Activity)"
  let pureRunning = filterByField name actRunning records
  let nonPureRunning = filterByField name actRunningNoAct records
  let running = pureRunning ++ nonPureRunning
  putStrLn $ "showing entries with name: Running and friends"
  putStrLn $ show $ amountStats running
  putStrLn $ show $ dateStats running
  putStrLn $ "Non-pure running records:"
  putStrLn $ show $ length nonPureRunning 


