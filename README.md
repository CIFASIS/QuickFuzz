# QuickFuzz

An experimental grammar fuzzer in Haskell using QuickCheck, Template Haskell and specific libraries from Hackage.
It found [some interesting bugs](http://QuickFuzz.org/).

## Authors

* Pablo Buiras (Chalmers University of Technology)
* Gustavo Grieco (CIFASIS-Conicet and VERIMAG)
* Martín Escarrá (UNR)

## Requirements

 * A modern version of GHC (e.g 7.10)
 * zlib-dev
 * libbz2-dev

In Ubuntu, a PPA to install GHC 7.10 is available [here](https://launchpad.net/~hvr/+archive/ubuntu/ghc).

## Instalation

    $ git clone https://github.com/CIFASIS/QuickFuzz.git
    $ cd QuickFuzz

Then, you may choose between a minimal installation (only Bmp, Png, Jpeg, Tiff and Gif supported) and
a complete one.

    **Minimal**
        $ ./install.sh -m
    **Complete**
        $ ./install.sh

Additionally, you may want to use QuickFuzz more easily by appending this line to your **~/.bashrc** file:

    export PATH=$HOME/.cabal/bin:$PATH
