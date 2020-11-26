import Text.Printf
import Control.Monad
import System.Pci

main :: IO ()
main = withAccess $ \acc -> do
  devices <- getDevices acc
  forM_ devices $ \device -> do
    void $ fillInfo device $ fillIdent <> fillBases <> fillClass
    c <- readByte device c'PCI_INTERRUPT_PIN
    d <- getDeviceDesc device
    printf
      "%04x:%02x:%02x.%d vendor=%04x device=%04x class=%04x irq=%d (pin %d) base0=%lx"
      (domain d) (bus d) (dev d) (func d) (vendorId d) (deviceId d) (deviceClass d)
      (irq d) c (head $ baseAddr d)
    name <- lookupName acc lookupDevice (fromIntegral $ vendorId d) (fromIntegral $ deviceId d) 0 0 
    printf " (%s)\n" name
