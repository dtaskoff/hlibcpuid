-----------------------------------------------------------------------------
-- |
-- Module      :  System.LibCPUID.CPUID
-- Copyright   :  (c) Daniel Taskoff, 2020
-- License     :  MIT
--
-- Maintainer  :  daniel.taskoff@gmail.com
-- Stability   :  experimental
--
-- A wrapper around __cpu_id_t__ from https://github.com/anrieff/libcpuid.
-----------------------------------------------------------------------------

module System.LibCPUID.CPUID
  (
  -- * 'CPUID'
    CPUID(..)
  ) where

import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt(..), CUChar(..))
import Foreign.Marshal (advancePtr)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))


-- | CPU information and features.
data CPUID = CPUID
  { vendorString :: String
    -- ^ CPU vendor string, e.g. \"GenuineIntel\".
  , brandString :: String
    -- ^ CPU brand string, e.g. \"Intel(R) Core(TM) i5-7500 CPU @ 3.40GHz\".
  , hasTSC :: Bool
    -- ^ Is a time-stamp counter available.
  , physicalCores :: Int
    -- ^ Number of physical cores per processor.
  , logicalCores :: Int
    -- ^ Number of logical cores per processor.
  , totalLogicalCores :: Int
    -- ^ Total number of logical cores, which is 'logicalCores' multiplied by the number of processors.
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
