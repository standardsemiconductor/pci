module System.Pci.Device.Fill where

import Data.Word
import Data.Bits
import Foreign.C.Types
import System.Pci.Device (Device, devicePtr)
import Bindings.Libpci.Pci

{-
From pci.h:
 * Most device properties take some effort to obtain, so libpci does not
 * initialize them during default bus scan. Instead, you have to call
 * pci_fill_info() with the proper PCI_FILL_xxx constants OR'ed together.
 *
 * Some properties are stored directly in the pci_dev structure.
 * The remaining ones can be accessed through pci_get_string_property().
 *
 * pci_fill_info() returns the current value of pci_dev->known_fields.
 * This is a bit mask of all fields, which were already obtained during
 * the lifetime of the device. This includes fields which are not supported
 * by the particular device -- in that case, the field is left at its default
 * value, which is 0 for integer fields and NULL for pointers. On the other
 * hand, we never consider known fields unsupported by the current back-end;
 * such fields always contain the default value.
 *
 * XXX: flags and the result should be unsigned, but we do not want to break the ABI.
-}
-- Fill info returns updated knownFields.
fillInfo :: Device -> Fill -> IO Word32
fillInfo device flags = fromIntegral <$> c'pci_fill_info (devicePtr device) (unFill flags)

newtype Fill = Fill { unFill :: CInt }
  deriving newtype (Eq, Num, Bits)

instance Semigroup Fill where
  (<>) = (.|.)

instance Monoid Fill where
  mempty = 0

fillIdent       :: Fill
fillIrq         :: Fill
fillBases       :: Fill
fillRomBase     :: Fill
fillSizes       :: Fill
fillClass       :: Fill
fillCaps        :: Fill
fillExtCaps     :: Fill
fillPhysSlot    :: Fill
fillModuleAlias :: Fill
fillLabel       :: Fill
fillNumaNode    :: Fill
fillIOFlags     :: Fill
--fillDTNode      :: Fill
fillRescan      :: Fill

fillIdent       = c'PCI_FILL_IDENT
fillIrq         = c'PCI_FILL_IRQ
fillBases       = c'PCI_FILL_BASES
fillRomBase     = c'PCI_FILL_ROM_BASE
fillSizes       = c'PCI_FILL_SIZES
fillClass       = c'PCI_FILL_CLASS
fillCaps        = c'PCI_FILL_CAPS
fillExtCaps     = c'PCI_FILL_EXT_CAPS
fillPhysSlot    = c'PCI_FILL_PHYS_SLOT
fillModuleAlias = c'PCI_FILL_MODULE_ALIAS
fillLabel       = c'PCI_FILL_LABEL
fillNumaNode    = c'PCI_FILL_NUMA_NODE
fillIOFlags     = c'PCI_FILL_IO_FLAGS
--fillDTNode      = c'PCI_FILL_DT_NODE
fillRescan      = c'PCI_FILL_RESCAN
