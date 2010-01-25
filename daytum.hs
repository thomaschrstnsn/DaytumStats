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
    Right xs -> print $ fromList $ xs !! 1

data DaytumRecord = Daytum String String Double [String]
                    deriving Show

-- | Creates a DaytumRecord from list of strings
fromList :: [String] -> DaytumRecord
fromList xs = Daytum name date value acts
  where
    [ns, ds, vs, as] = xs
    name = ns
    date = ds
    value = (read vs)::Double
    acts = [as]