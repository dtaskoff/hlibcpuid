module Main where

import System.LibCPUID
import System.LibCPUID.TSC


main :: IO ()
main = do
  putStrLn "------------------------------------------"
  putStrLn "Available CPU information"
  putStrLn "------------------------------------------"
  exampleCPUID

  putStrLn "------------------------------------------"
  putStrLn "Measuring CPU clock frequency"
  putStrLn "------------------------------------------"
  exampleTSC

exampleCPUID :: IO ()
exampleCPUID = cpuid >>= \case
  Left err -> error err
  Right CPUID {..} -> do
    mapM_ putStrLn
      [ "vendor string: " ++ vendorString
      , "brand string: " ++ brandString
      , "has a time-stamp counter (TSC): " ++  if hasTSC then "yes" else "no"
      , "# physical cores per processor: " ++ show physicalCores
      , "# logical cores per processor: " ++ show logicalCores
      , "# processors: " ++ show (div totalLogicalCores logicalCores)
      ]

exampleTSC :: IO ()
exampleTSC = do
  tscMark <- initialise

  mark tscMark
  let fib = scanl (+) 0 (1:fib)
  putStrLn $ "The 1000th number in the Fibonacci sequence is " ++ show @Int (last (take 1000 fib))
  unmark tscMark
  clock <- clockBy tscMark

  putStrLn $ "calculated @" ++ show clock ++ "MHz"
