module System.Pci.Device
  ( Device
  , devicePtr
  , getDevices
  , readByte
  , readWord
  , readLong
  , readBlock
  , readVpd
  , writeByte
  , writeWord
  , writeLong
  , writeBlock
  ) where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import System.Pci.Access
import Bindings.Libpci.Pci

data Device = Device
  { access      :: !Access
  , devicePtr   :: !(Ptr C'pci_dev)
  , next        :: !(Ptr C'pci_dev)
  }

mkDevice :: Access -> Ptr C'pci_dev -> IO Device
mkDevice acc pciDevPtr = do
  pciDev <- peek pciDevPtr
  return $ Device
    { access    = acc
    , devicePtr = pciDevPtr
    , next      = c'pci_dev'next pciDev
    }

getDevices :: Access -> IO [Device]
getDevices acc = do
  c'pci_scan_bus $ accessPtr acc
  devPtr <- peek $ p'pci_access'devices $ accessPtr acc
  go devPtr
  where
    go :: Ptr C'pci_dev -> IO [Device]
    go devPtr
      | devPtr == nullPtr = return []
      | otherwise = do
          d <- mkDevice acc devPtr
          (d :) <$> go (next d)

readByte :: Device -> CInt -> IO Word8
readByte device = fmap fromIntegral . c'pci_read_byte (devicePtr device)

readWord :: Device -> CInt -> IO Word16
readWord device = fmap fromIntegral . c'pci_read_word (devicePtr device)

readLong :: Device -> CInt -> IO Word32
readLong device = fmap fromIntegral . c'pci_read_long (devicePtr device)

readBlock :: Device -> CInt -> Int -> IO (Either String [Word8])
readBlock device pos len = allocaArray len $ \buf -> do
  status <- c'pci_read_block (devicePtr device) pos buf $ fromIntegral len
  if status == 1
    then Right . fmap fromIntegral <$> peekArray len buf
    else return $ Left "readBlock error"

readVpd :: Device -> CInt -> Int -> IO (Either String [Word8])
readVpd device pos len = allocaArray len $ \buf -> do
  status <- c'pci_read_vpd (devicePtr device) pos buf $ fromIntegral len
  if status == 1
    then Right . fmap fromIntegral <$> peekArray len buf
    else return $ Left "readVpd error"

writeByte :: Device -> CInt -> Word8 -> IO (Maybe String)
writeByte device pos byte = do
  status <- c'pci_write_byte (devicePtr device) pos $ fromIntegral byte
  return $ if status == 1
    then Nothing
    else Just "writeByte error"

writeWord :: Device -> CInt -> Word16 -> IO (Maybe String)
writeWord device pos word = do
  status <- c'pci_write_word (devicePtr device) pos $ fromIntegral word
  return $ if status == 1
    then Nothing
    else Just "writeWord error"

writeLong :: Device -> CInt -> Word32 -> IO (Maybe String)
writeLong device pos long = do
  status <- c'pci_write_long (devicePtr device) pos $ fromIntegral long
  return $ if status == 1
    then Nothing
    else Just "writeLong error"

writeBlock :: Device -> CInt -> [Word8] -> IO (Maybe String)
writeBlock device pos block = allocaArray len $ \bufPtr -> do
  pokeArray bufPtr $ fmap fromIntegral block
  status <- c'pci_write_block (devicePtr device) pos bufPtr $ fromIntegral len
  return $ if status == 1
    then Nothing
    else Just "writeBlock error"
  where
    len = length block
