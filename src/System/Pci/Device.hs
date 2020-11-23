module System.Pci.Device
  ( Device
  , devicePtr
  , getDevices
  , readByte
  , bus
  , dev
  , func
  , vendorId
  , deviceId
  , deviceClass
  , irq
  , baseAddr
  , domain
  ) where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Pci.Access
import Bindings.Libpci.Pci

data Device = Device
  { access      :: !Access
  , devicePtr   :: !(Ptr C'pci_dev)
  , next        :: !(Ptr C'pci_dev)
  , domain16    :: !Word16
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

mkDevice :: Access -> Ptr C'pci_dev -> IO Device
mkDevice acc pciDevPtr = do
  pciDev <- peek pciDevPtr
  return $ Device
    { access      = acc
    , devicePtr   = pciDevPtr
    , next        = c'pci_dev'next         pciDev
    , domain16    = fromIntegral $ c'pci_dev'domain_16    pciDev
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

getDevices :: Access -> IO [Device]
getDevices acc = do
  c'pci_scan_bus $ accessPtr acc
  devPtr <- c'pci_access'devices <$> peek (accessPtr acc)
  go devPtr
  where
    go :: Ptr C'pci_dev -> IO [Device]
    go devPtr
      | devPtr == nullPtr = return []
      | otherwise = do
          d <- mkDevice acc devPtr
          (d :) <$> go (next d)


readByte :: Device -> CInt -> IO Word8
readByte device = fmap fromIntegral . c'pci_read_byte (devicePtr device)
