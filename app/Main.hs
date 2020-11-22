module Main where

import Control.Exception (finally)
import Data.Bits ((.|.))
import Data.Word (Word8, Word16, Word32)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Foreign.C.Types
import Text.Printf
import Control.Monad

import Bindings.Libpci.Pci
import Bindings.Libpci.Header

main :: IO ()
main = do
  putStrLn "Hello, PCI!"
  withAccess $ \access -> do
    putStrLn "Using PCI!"
    devices <- getDevices access
    forM_ devices $ \d -> printf "device=%x\n" (deviceId d)
{-
    c'pci_fill_info devices fillFlags
    c <- c'pci_read_byte devices c'PCI_INTERRUPT_PIN
    print c
    vendorId <- c'vendor_id devices
    deviceId <- c'device_id devices
    name <- c'pci_lookup_name pacc namebuf (sizeOf nameBuf) c'PCI_LOOKUP_DEVICE vendorId deviceId
    print name
-}
  putStrLn "Goodbye, PCI!"
  where
    fillFlags :: CInt
    fillFlags = c'PCI_FILL_IDENT .|. c'PCI_FILL_BASES .|. c'PCI_FILL_CLASS
  
newtype Access = Access { accessPtr :: Ptr C'pci_access }

withAccess :: (Access -> IO a) -> IO a
withAccess k = do
  pacc <- c'pci_alloc
  c'pci_init pacc
  o <- k $ Access pacc
  c'pci_cleanup pacc
  return o

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
  c'pci_scan_bus $ accessPtr acc
  devPtr <- c'pci_access'devices <$> peek (accessPtr acc)
  go devPtr
  where
    go :: Ptr C'pci_dev -> IO [Device]
    go devPtr
      | devPtr == nullPtr = return []
      | otherwise = do
          dev <- mkDevice acc devPtr
          (dev :) <$> go (next dev)

{-
 * Most device properties take some effort to obtain, so libpci does not
 * initialize them during default bus scan. Instead, you have to call
 * pci_fill_info() with the proper PCI_FILL_xxx constants OR'ed together.
 *
 * Some properties are stored directly in the pci_dev structure.
 * The remaining ones can be accessed through pci_get_string_property().
 *
 * pci_fill_info() returns the current value of pci_dev->known_fields.
 * This is a bit mask of all fields, which were already obtained during
 * the lifetime of the device. This includes fields which are not supported
 * by the particular device -- in that case, the field is left at its default
 * value, which is 0 for integer fields and NULL for pointers. On the other
 * hand, we never consider known fields unsupported by the current back-end;
 * such fields always contain the default value.
 *
 * XXX: flags and the result should be unsigned, but we do not want to break the ABI.
-}
-- Fill info returns updated knownFields.
fillInfo :: Device -> CInt -> IO Word32
fillInfo device flags = fromIntegral <$> c'pci_fill_info (devicePtr device) flags

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
    , domain      = fromIntegral $ c'pci_dev'domain       pciDev
    }

