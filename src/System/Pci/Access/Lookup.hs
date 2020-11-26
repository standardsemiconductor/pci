module System.Pci.Access.Lookup
  ( lookupName
  , setNameListPath
  , idCacheFlush
  , LookupMode
  , lookupVendor
  , lookupDevice
  , lookupClass
  , lookupSubsystem
  , lookupProgif
  , lookupNumeric
  , lookupNoNumbers
  , lookupMixed
  , lookupNetwork
  , lookupSkipLocal
  , lookupCache
  , lookupRefreshCache
  , lookupNoHwdb
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Data.Bits ((.|.))
import Bindings.Libpci.Pci
import System.Pci.Access (Access, accessPtr)

-- From pci.h
{-
/*
 *	Conversion of PCI ID's to names (according to the pci.ids file)
 *
 *	Call pci_lookup_name() to identify different types of ID's:
 *
 *	VENDOR				(vendorID) -> vendor
 *	DEVICE				(vendorID, deviceID) -> device
 *	VENDOR | DEVICE			(vendorID, deviceID) -> combined vendor and device
 *	SUBSYSTEM | VENDOR		(subvendorID) -> subsystem vendor
 *	SUBSYSTEM | DEVICE		(vendorID, deviceID, subvendorID, subdevID) -> subsystem device
 *	SUBSYSTEM | VENDOR | DEVICE	(vendorID, deviceID, subvendorID, subdevID) -> combined subsystem v+d
 *	SUBSYSTEM | ...			(-1, -1, subvendorID, subdevID) -> generic subsystem
 *	CLASS				(classID) -> class
 *	PROGIF				(classID, progif) -> programming interface
 */
-}
lookupName :: Access -> LookupMode -> CInt -> CInt -> CInt -> CInt -> IO String
lookupName acc (LookupMode lm) w x y z = allocaBytes 1024 $ \nb ->
  peekCString =<< c'pci_lookup_name (accessPtr acc) nb 1024 lm w x y z

setNameListPath :: Access -> String -> Int -> IO ()
setNameListPath acc name toBeFreed = withCString name $ \nameCStr ->
  c'pci_set_name_list_path (accessPtr acc) nameCStr $ fromIntegral toBeFreed

idCacheFlush :: Access -> IO ()
idCacheFlush = c'pci_id_cache_flush . accessPtr

newtype LookupMode = LookupMode { unLookupMode :: C'pci_lookup_mode }

instance Semigroup LookupMode where
  lm1 <> lm2 = LookupMode $ unLookupMode lm1 .|. unLookupMode lm2

instance Monoid LookupMode where
  mempty = LookupMode 0

lookupVendor       :: LookupMode
lookupDevice       :: LookupMode
lookupClass        :: LookupMode
lookupSubsystem    :: LookupMode
lookupProgif       :: LookupMode
lookupNumeric      :: LookupMode
lookupNoNumbers    :: LookupMode
lookupMixed        :: LookupMode
lookupNetwork      :: LookupMode
lookupSkipLocal    :: LookupMode
lookupCache        :: LookupMode
lookupRefreshCache :: LookupMode
lookupNoHwdb       :: LookupMode

lookupVendor       = LookupMode c'PCI_LOOKUP_VENDOR
lookupDevice       = LookupMode c'PCI_LOOKUP_DEVICE
lookupClass        = LookupMode c'PCI_LOOKUP_CLASS
lookupSubsystem    = LookupMode c'PCI_LOOKUP_SUBSYSTEM
lookupProgif       = LookupMode c'PCI_LOOKUP_PROGIF
lookupNumeric      = LookupMode c'PCI_LOOKUP_NUMERIC
lookupNoNumbers    = LookupMode c'PCI_LOOKUP_NO_NUMBERS
lookupMixed        = LookupMode c'PCI_LOOKUP_MIXED
lookupNetwork      = LookupMode c'PCI_LOOKUP_NETWORK
lookupSkipLocal    = LookupMode c'PCI_LOOKUP_SKIP_LOCAL
lookupCache        = LookupMode c'PCI_LOOKUP_CACHE
lookupRefreshCache = LookupMode c'PCI_LOOKUP_REFRESH_CACHE
lookupNoHwdb       = LookupMode c'PCI_LOOKUP_NO_HWDB
