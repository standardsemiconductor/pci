![Haskell CI](https://github.com/standardsemiconductor/pci/workflows/Haskell%20CI/badge.svg)
# pci - Work In Progress
A library for accessing PCI devices.

## Build
```
cabal build --constraint="bindings-libpci +libpci-vvv"
```

## Example
```
cabal run --constraint="bindings-libpci +libpci-vvv"
```
This will print a description of each connected PCI device.
