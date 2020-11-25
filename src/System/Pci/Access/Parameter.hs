module System.Pci.Access.Parameter where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Bindings.Libpci.Pci
import System.Pci.Access (Access, accessPtr)

getParam :: Access -> String -> IO String
getParam acc p = withCString p $ \pCStr ->
  peekCString =<< c'pci_get_param (accessPtr acc) pCStr
  
setParam :: Access -> String -> String -> IO (Maybe String)
setParam acc p v = withCString p $ \pCStr ->
  withCString v $ \vCStr -> do
    status <- c'pci_set_param (accessPtr acc) pCStr vCStr
    return $ if status == 0
      then Nothing
      else Just "No such parameter"

newtype Param = Param { paramPtr :: Ptr C'pci_param }

param :: Param -> IO String
param = (peekCString =<<) . peek . p'pci_param'param . paramPtr

value :: Param -> IO String
value = (peekCString =<<) . peek . p'pci_param'value . paramPtr

help :: Param -> IO String
help = (peekCString =<<) . peek . p'pci_param'help . paramPtr
    
listParams :: Access -> IO [Param]
listParams acc = go nullPtr
  where
    go prevPtr = do
      pPtr <- c'pci_walk_params (accessPtr acc) prevPtr
      (Param pPtr :) <$> go pPtr

