module System.Pci.Device
  ( Device
  , devicePtr
  , getDevices
  , readByte
  , readWord
  , readLong
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
  }

mkDevice :: Access -> Ptr C'pci_dev -> IO Device
mkDevice acc pciDevPtr = do
  pciDev <- peek pciDevPtr
  return $ Device
    { access      = acc
    , devicePtr   = pciDevPtr
    , next        = c'pci_dev'next pciDev
    }

getDevices :: Access -> IO [Device]
getDevices acc = do
  c'pci_scan_bus $ accessPtr acc
  devPtr <- peek $ p'pci_access'devices $ accessPtr acc
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

readWord :: Device -> CInt -> IO Word16
readWord device = fmap fromIntegral . c'pci_read_word (devicePtr device)

readLong :: Device -> CInt -> IO Word32
readLong device = fmap fromIntegral . c'pci_read_long (devicePtr device)

