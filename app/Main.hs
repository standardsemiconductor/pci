module Main where

import Control.Exception (finally)
import Data.Bits ((.|.))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

import Bindings.Libpci.Pci
import Bindings.Libpci.Header

main :: IO ()
main = do
  putStrLn "Hello, PCI!"
  withAccess $ \access -> do
    putStrLn "Using PCI!"
    c'pci_scan_bus pacc
    devices <- c'pci_access'devices <$> peek pacc
    c'pci_fill_info devices fillFlags
    c <- c'pci_read_byte devices c'PCI_INTERRUPT_PIN
    print c
    vendorId <- c'vendor_id devices
    deviceId <- c'device_id devices
    name <- c'pci_lookup_name pacc namebuf (sizeOf nameBuf) c'PCI_LOOKUP_DEVICE vendorId deviceId
    print name
  putStrLn "Goodbye, PCI!"
  where
    fillFlags = c'PCI_FILL_IDENT .|. c'PCI_FILL_BASES .|. c'PCI_FILL_CLASS
  
newtype Access = Access { accessPtr :: !(Ptr C'pci_access) }

withAccess :: (Access -> IO a) -> IO a
withAccess k = do
  pacc <- c'pci_alloc
  c'pci_init pacc
  k $ PCIAccess pacc
  c'pci_cleanup pacc

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
  , domain      :: !Int
  }

getDevices :: Access -> IO [Device]
getDevices acc = do
  c'pci_scan_bus $ acessPtr acc
  devPtr <- c'pci_access'devices <$> peek (accessPtr acc)
  device <- mkDevice acc devPtr
  (device :) <$> go (next device)
  where
    go :: Ptr C'pci_dev -> IO [Device]
    go devPtr
      | devPtr == nullPtr = return []
      | otherwise = do
          dev <- mkDevice acc devPtr
          (dev :) <$> go (next dev)

mkDevice :: Access -> Ptr C'pci_dev -> IO Device
mkDevice acc pciDevPtr = do
  pciDev <- peek pciDevPtr
  return $ Device
    { access      = acc
    , devicePtr   = pciDevPtr
    , next        = c'pci_dev'next         pciDev
    , domain16    = c'pci_dev'domain_16    pciDev
    , bus         = c'pci_dev'bus          pciDev
    , dev         = c'pci_dev'dev          pciDev
    , func        = c'pci_dev'func         pciDev
    , knownFields = c'pci_dev'known_fields pciDev
    , vendorId    = c'pci_dev'vendor_id    pciDev
    , deviceId    = c'pci_dev'device_id    pciDev
    , deviceClass = c'pci_dev'device_class pciDev
    , irq         = c'pci_dev'irq          pciDev
    , domain      = c'pci_dev'domain       pciDev
    }

