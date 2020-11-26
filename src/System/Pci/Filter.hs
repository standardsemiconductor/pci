module System.Pci.Filter where

import System.Pci.Access (Access, accessPtr)
import System.Pci.Device (Device, devicePtr)
import Bindings.Libpci.Pci
import Foreign.C.String
import Foreign.Ptr
import Control.Monad

newtype Filter = Filter { filterPtr :: Ptr C'pci_filter }

filterInit :: Access -> Filter -> IO ()
filterInit acc fil = c'pci_filter_init (accessPtr acc) $ filterPtr fil

filterParseSlot :: Filter -> String -> IO String
filterParseSlot fil str = withCString str $ \cStr ->
  peekCString =<< c'pci_filter_parse_slot (filterPtr fil) cStr

filterParseId :: Filter -> String -> IO String
filterParseId fil str = withCString str $
  c'pci_filter_parse_id (filterPtr fil) >=> peekCString

filterMatch :: Filter -> Device -> IO Int
filterMatch fil device = fmap fromIntegral $ c'pci_filter_match (filterPtr fil) (devicePtr device)

