module System.Pci.Access
  ( Access
  , accessPtr
  , withAccess
  , scanBus
  , lookupMethod
  , getMethodName
  ) where

import Control.Exception (finally)
import Foreign.Ptr
import Foreign.C.String
import Bindings.Libpci.Pci

newtype Access = Access { accessPtr :: Ptr C'pci_access }

withAccess :: (Access -> IO a) -> IO a
withAccess k = do
  pacc <- c'pci_alloc
  c'pci_init pacc
  finally (k (Access pacc)) (c'pci_cleanup pacc)

-- Scanning of devices
scanBus :: Access -> IO ()
scanBus = c'pci_scan_bus . accessPtr

lookupMethod :: String -> IO (Either String Int)
lookupMethod name = do
  n <- fmap fromIntegral $ withCString name $ c'pci_lookup_method
  return $ if n == negate 1
    then Left "not found"
    else Right n

getMethodName :: Int -> IO (Either String String)
getMethodName index = do
  cStr <- c'pci_get_method_name $ fromIntegral index
  if cStr == nullPtr
    then return $ Left "index out of range"
    else Right <$> peekCString cStr
      
