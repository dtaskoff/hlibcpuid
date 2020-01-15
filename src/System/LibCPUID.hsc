module System.LibCPUID
  ( isCPUIDPresent
  ) where

import Foreign.C.Types (CInt(..))


-- | Check if the CPUID instruction is supported
isCPUIDPresent :: IO Bool
isCPUIDPresent = (== 1) <$> c_cpuid_present

foreign import ccall "cpuid_present"
  c_cpuid_present :: IO CInt
