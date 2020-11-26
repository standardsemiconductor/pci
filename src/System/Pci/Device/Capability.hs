module System.Pci.Device.Capability where

import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Bindings.Libpci.Pci
import System.Pci.Device (Device, devicePtr)

newtype Capability = Capability { capabilityPtr :: Ptr C'pci_cap }

findCap :: Device -> Word32 -> Word32 -> IO Capability
findCap device capId capType = Capability <$> c'pci_find_cap (devicePtr device) (fromIntegral capId) (fromIntegral capType)

findCapNR :: Device -> Word32 -> Word32 -> Ptr CUInt -> IO Capability
findCapNR device capId capType capNumber = Capability <$> c'pci_find_cap_nr (devicePtr device) (fromIntegral capId) (fromIntegral capType) capNumber 
