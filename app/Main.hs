module Main where

import Control.Exception (finally)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)

import Bindings.Libpci

main :: IO ()
main = do
  putStrLn "Hello, PCI!"
  withPCI $ const $ putStrLn "Using PCI!"
  putStrLn "Goodbye, PCI!"
  
withPCI :: (Ptr C'pci_access -> IO a) -> IO a
withPCI exec = alloca $ \pPCIAccess -> do
  c'pci_init pPCIAccess
  finally (exec pPCIAccess) (c'pci_cleanup pPCIAccess)
