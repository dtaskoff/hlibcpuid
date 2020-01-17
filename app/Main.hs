module Main where

import System.LibCPUID


main :: IO ()
main = cpuid >>= \case
  Left err -> error err
  Right CPUID {..} -> do
    mapM_ putStrLn
      [ "Available CPU information"
      , "------------------------------------------"
      , "vendor string: " ++ vendorString
      , "brand string: " ++ brandString
      , "has a time-stamp counter (TSC): " ++  if hasTSC then "yes" else "no"
      , "# physical cores per processor: " ++ show physicalCores
      , "# logical cores per processor: " ++ show logicalCores
      , "# processors: " ++ show (div totalLogicalCores logicalCores)
      ]
