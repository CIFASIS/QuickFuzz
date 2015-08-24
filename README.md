# QuickFuzz

An experimental grammar fuzzer in Haskell using QuickCheck, Template Haskell and specific libraries from Hackage.
It found interesting bugs in gdk-pixbuf, jasper and unzip.

## Authors

* Pablo Buiras (Chalmers University of Technology)
* Gustavo Grieco (CIFASIS-Conicet and VERIMAG)
* Martín Escarrá (UNR)

## Requirements

 * A modern version of GHC (e.g 7.10)
 * happy


In Ubuntu, a PPA to install GHC 7.10 is available [here](https://launchpad.net/~hvr/+archive/ubuntu/ghc).

## Instalation

  1. Install the modified version of the packages (using "caball install" each subdirectory):
    * Juicypixels (3.2.5.2)
    * tar (0.4.2.1)
  2. Install QuickFuzz (using "cabal install" inside the root of this repository)
