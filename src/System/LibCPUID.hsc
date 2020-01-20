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
-- Currently implemented:
--
-- * getting CPU vendor and brand strings, TSC availability, number of physical, and logical cores
-- * measuring CPU clock frequency using TSC
-----------------------------------------------------------------------------

module System.LibCPUID
  (
  -- * LibCPUID utilities
    getTotalLogicalCores
  -- * Reexports from "System.LibCPUID.CPUID"
  , module System.LibCPUID.CPUID
  -- * Reexports from "System.LibCPUID.TSC"
  , module System.LibCPUID.TSC
  ) where

import Foreign.C.Types (CInt(..))
import System.LibCPUID.CPUID (CPUID(..), cpuid)
import System.LibCPUID.TSC (TSCMark, initialise, mark, unmark, clockBy)


#include "libcpuid.h"

-- | Get the total number of logical cores (even if CPUID is not present).
-- If CPUID is present, the following is true:
--
-- * @'getTotalLogicalCores' = 'totalLogicalCores' '<$>' 'cpuid'@
getTotalLogicalCores :: IO Int
getTotalLogicalCores = fromIntegral <$> c_cpuid_get_total_cpus

foreign import ccall "cpuid_get_total_cpus"
  c_cpuid_get_total_cpus :: IO CInt
