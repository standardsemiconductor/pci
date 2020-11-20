module Main where

import Control.Exception (finally)
import Data.Bits ((.|.))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

import Bindings.Libpci.Pci
import Bindings.Libpci.Header

main :: IO ()
main = do
  putStrLn "Hello, PCI!"
  pacc <- c'pci_alloc
  c'pci_init pacc
  
  putStrLn "Using PCI!"
  c'pci_scan_bus pacc
  devices <- c'pci_access'devices <$> peek pacc
  c'pci_fill_info devices fillFlags
  c <- c'pci_read_byte devices c'PCI_INTERRUPT_PIN
  print c
  
  putStrLn "Goodbye, PCI!"
  c'pci_cleanup pacc
  where
    fillFlags = c'PCI_FILL_IDENT .|. c'PCI_FILL_BASES .|. c'PCI_FILL_CLASS
  
