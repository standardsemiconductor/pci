module System.Pci.Access.Lookup
  ( lookupName
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

lookupName :: Access -> LookupMode -> CInt -> CInt -> CInt -> CInt -> IO String
lookupName acc (LookupMode lm) w x y z = allocaBytes 1024 $ \nb ->
  peekCString =<< c'pci_lookup_name (accessPtr acc) nb 1024 lm w x y z

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
