-----------------------------------------------------------------------------
-- |
-- Module      :  System.LibCPUID
-- Copyright   :  (c) Daniel Taskoff, 2020
-- License     :  MIT
--
-- Maintainer  :  daniel.taskoff@gmail.com
-- Stability   :  experimental
--
-- Bindings to https://github.com/anrieff/libcpuid.
--
-- Currently implemented features:
--
-- > cpuid >>= \case
-- >  Left err -> error err
-- >  Right CPUID {..} -> do
-- >    mapM_ putStrLn
-- >      [ "Available CPU information"
-- >      , "------------------------------------------"
-- >      , "vendor string: " ++ vendorString
-- >      , "brand string: " ++ brandString
-- >      , "has a time-stamp counter (TSC): " ++  if hasTSC then "yes" else "no"
-- >      , "# physical cores per processor: " ++ show physicalCores
-- >      , "# logical cores per processor: " ++ show logicalCores
-- >      , "# processors: " ++ show (div totalLogicalCores logicalCores)
-- >      ]
-----------------------------------------------------------------------------

module System.LibCPUID
  (
  -- * LibCPUID
    cpuid
  , isCPUIDPresent
  , getTotalLogicalCores
  -- * Reexports
  , CPUID(..)
  ) where

import Foreign.C.Types (CInt(..))
import Foreign.Marshal (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))
import System.LibCPUID.CPUID (CPUID(..))


#include "libcpuid.h"

-- | Get CPU information and features, or an error message, if the CPU can't be identified by libcpuid.
cpuid :: IO (Either String CPUID)
cpuid = do
  allocaBytes #{size struct cpu_raw_data_t} \cpu_raw_data_t_ptr -> do
    res <- c_cpuid_get_raw_data cpu_raw_data_t_ptr
    case toMaybeError res of
      Just err -> pure $ Left err
      Nothing ->
        allocaBytes #{size struct cpu_id_t} \cpu_id_t_ptr -> do
          res' <- c_cpu_identify cpu_raw_data_t_ptr cpu_id_t_ptr
          case toMaybeError res' of
            Just err -> pure $ Left err
            Nothing -> Right <$> peek cpu_id_t_ptr

foreign import ccall "cpuid_get_raw_data"
  c_cpuid_get_raw_data :: Ptr cpu_raw_data_t -> IO CInt

foreign import ccall "cpu_identify"
  c_cpu_identify :: Ptr cpu_raw_data_r -> Ptr cpu_id_t -> IO CInt

toMaybeError :: CInt -> Maybe String
toMaybeError = \case
  #{const ERR_OK} -> Nothing
  err -> Just
    case err of
      #{const ERR_NO_CPUID} -> "CPUID instruction is not supported"
      #{const ERR_NO_RDTSC} -> "RDTSC instruction is not supported"
      #{const ERR_NO_MEM} -> "Memory allocation failed"
      #{const ERR_OPEN} -> "File open operation failed"
      #{const ERR_BADFMT} -> "Bad file format"
      #{const ERR_NOT_IMP} -> "Not implemented"
      #{const ERR_CPU_UNKN} -> "Unsupported processor"
      #{const ERR_NO_RDMSR} -> "RDMSR instruction is not supported"
      #{const ERR_NO_DRIVER} -> "RDMSR driver error (generic)"
      #{const ERR_NO_PERMS} -> "No permissions to install RDMSR driver"
      #{const ERR_EXTRACT} -> "Cannot extract RDMSR driver (read only media?)"
      #{const ERR_HANDLE} -> "Bad handle"
      #{const ERR_INVMSR} -> "Invalid MSR"
      #{const ERR_INVCNB} -> "Invalid core number"
      #{const ERR_HANDLE_R} -> "Error on handle read"
      #{const ERR_INVRANGE} -> "Invalid given range"
      _ -> "Unknown error"

-- | Get the total number of logical cores (even if CPUID is not present).
-- If CPUID is present, the following is true:
--
-- * @'getTotalLogicalCores' = 'totalLogicalCores' '<$>' 'cpuid'@
getTotalLogicalCores :: IO Int
getTotalLogicalCores = fromIntegral <$> c_cpuid_get_total_cpus

foreign import ccall "cpuid_get_total_cpus"
  c_cpuid_get_total_cpus :: IO CInt

-- | Check if the CPUID instruction is supported.
isCPUIDPresent :: IO Bool
isCPUIDPresent = (== 1) <$> c_cpuid_present

foreign import ccall "cpuid_present"
  c_cpuid_present :: IO CInt
