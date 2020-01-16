module System.LibCPUID
  (
  -- * 'CPUID'
    CPUID(..), cpuid
  , getTotalLogicalCores
  , isCPUIDPresent
  ) where

import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt(..), CUChar(..))
import Foreign.Marshal (allocaBytes, advancePtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))


-- | CPU information and features.
data CPUID = CPUID
  { vendorString :: String
  , brandString :: String
  , hasTSC :: Bool
  , physicalCores :: Int
  , logicalCores :: Int
  , totalLogicalCores :: Int
  }

#include "libcpuid.h"

instance Storable CPUID where
  alignment _ = #{alignment struct cpu_id_t}
  sizeOf _ = #{size struct cpu_id_t}
  peek ptr = do
    vendorString <- peekCString $ plusPtr ptr #{offset struct cpu_id_t, vendor_str}
    brandString <- peekCString $ plusPtr ptr #{offset struct cpu_id_t, brand_str}
    hasTSC <-
      let ptr' = advancePtr (castPtr @CPUID @CUChar ptr) #{const CPU_FEATURE_TSC}
       in (/= 0) <$> peekElemOff ptr' #{offset struct cpu_id_t, flags}
    physicalCores <- fromIntegral @CInt <$> #{peek struct cpu_id_t, num_cores} ptr
    logicalCores <- fromIntegral @CInt <$> #{peek struct cpu_id_t, num_logical_cpus} ptr
    totalLogicalCores <- fromIntegral @CInt <$> #{peek struct cpu_id_t, total_logical_cpus} ptr

    pure CPUID {..}
  poke _ _ = error "CPUID is read-only"

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
getTotalLogicalCores :: IO Int
getTotalLogicalCores = fromIntegral <$> c_cpuid_get_total_cpus

foreign import ccall "cpuid_get_total_cpus"
  c_cpuid_get_total_cpus :: IO CInt

-- | Check if the CPUID instruction is supported.
isCPUIDPresent :: IO Bool
isCPUIDPresent = (== 1) <$> c_cpuid_present

foreign import ccall "cpuid_present"
  c_cpuid_present :: IO CInt
