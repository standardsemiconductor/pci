module System.Pci.Access
  ( Access
  , accessPtr
  , withAccess
  ) where

import Control.Exception (finally)
import Foreign.Ptr
import Bindings.Libpci.Pci

newtype Access = Access { accessPtr :: Ptr C'pci_access }

withAccess :: (Access -> IO a) -> IO a
withAccess k = do
  pacc <- c'pci_alloc
  c'pci_init pacc
  finally (k (Access pacc)) (c'pci_cleanup pacc)

