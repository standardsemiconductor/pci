module System.Pci.Device.Description where

import Data.Word
import Foreign.Storable
import Bindings.Libpci.Pci
import System.Pci.Device (Device, devicePtr)


data DeviceDesc = DeviceDesc
  { domain16    :: !Word16
  , bus         :: !Word8
  , dev         :: !Word8
  , func        :: !Word8
  , knownFields :: !Word32
  , vendorId    :: !Word16
  , deviceId    :: !Word16
  , deviceClass :: !Word16
  , irq         :: !Int
  , baseAddr    :: ![Word64]
  , domain      :: !Int
  }

getDeviceDesc :: Device -> IO DeviceDesc
getDeviceDesc device = do
  pciDev <- peek $ devicePtr device
  return $ DeviceDesc
    { domain16    = fromIntegral $ c'pci_dev'domain_16    pciDev
    , bus         = fromIntegral $ c'pci_dev'bus          pciDev
    , dev         = fromIntegral $ c'pci_dev'dev          pciDev
    , func        = fromIntegral $ c'pci_dev'func         pciDev
    , knownFields = fromIntegral $ c'pci_dev'known_fields pciDev
    , vendorId    = fromIntegral $ c'pci_dev'vendor_id    pciDev
    , deviceId    = fromIntegral $ c'pci_dev'device_id    pciDev
    , deviceClass = fromIntegral $ c'pci_dev'device_class pciDev
    , irq         = fromIntegral $ c'pci_dev'irq          pciDev
    , baseAddr    = fromIntegral <$> c'pci_dev'base_addr  pciDev
    , domain      = fromIntegral $ c'pci_dev'domain       pciDev
    }
