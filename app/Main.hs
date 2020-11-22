module Main where

import Control.Exception (finally)
import Data.Bits ((.|.))
import Data.Word
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Text.Printf
import Control.Monad

import Bindings.Libpci.Pci
import Bindings.Libpci.Header

main :: IO ()
main = withAccess $ \acc -> do
  devices <- getDevices acc
  forM_ devices $ \d -> do
    void $ fillInfo d $ c'PCI_FILL_IDENT .|. c'PCI_FILL_BASES .|. c'PCI_FILL_CLASS
    c <- readByte d c'PCI_INTERRUPT_PIN
    printf
      "%04x:%02x:%02x.%d vendor=%04x device=%04x class=%04x irq=%d (pin %d) base0=%lx"
      (domain d) (bus d) (dev d) (func d) (vendorId d) (deviceId d) (deviceClass d)
      (irq d) c (head $ baseAddr d)
    name <- lookupName acc d $ c'PCI_LOOKUP_DEVICE
    printf " (%s)\n" name
  
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

readByte :: Device -> CInt -> IO Word8
readByte device = fmap fromIntegral . c'pci_read_byte (devicePtr device)

lookupName :: Access -> Device -> CInt -> IO String
lookupName acc device flags = allocaBytes 1024 $ \nb ->
  peekCString =<< c'pci_lookup_name (accessPtr acc) nb 1024 flags vid did
  where
    vid = fromIntegral $ vendorId device
    did = fromIntegral $ deviceId device
