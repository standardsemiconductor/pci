module System.Pci.Access.Parameter where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Bindings.Libpci.Pci
import System.Pci.Access (Access, accessPtr)

data Param = Param
  { param :: String
  , value :: String
  , help  :: String
  } deriving Show

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
    
getParams :: Access -> IO [Param]
getParams acc = go nullPtr
  where
    go prevPtr = do
      paramPtr <- c'pci_walk_params (accessPtr acc) prevPtr
      (:) <$> mkParam paramPtr <*> go paramPtr
    mkParam ptr = do
      p <- peekCString =<< peek (p'pci_param'param ptr)
      v <- peekCString =<< peek (p'pci_param'value ptr)
      h <- peekCString =<< peek (p'pci_param'help  ptr)
      return $ Param p v h
