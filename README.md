# QuickFuzz

An experimental grammar fuzzer in Haskell using QuickCheck, Template Haskell and specific libraries from Hackage.
There is more information in its [website](http://QuickFuzz.org/).

## Authors

* Pablo **Buiras** ([Chalmers University of Technology](http://www.chalmers.se/en/Pages/default.aspx))
* Martín **Ceresa** ([CIFASIS-Conicet](http://cifasis-conicet.gov.ar/))
* Gustavo **Grieco** ([CIFASIS-Conicet](http://cifasis-conicet.gov.ar/) and [VERIMAG](http://www-verimag.imag.fr/?lang=en))

## Students

* Franco Costantini

## Former Members

* Martín **Escarrá** ([Universidad Nacional de Rosario](http://www.unr.edu.ar/))

## Installation

### Stack

We **support** [Stack](www.haskellstack.org) to compile and install QuickFuzz. Before starting with it, make sure you have libgmp-dev installed otherwise ghc will fail to compile. Also, zlib.h is required to compile QuickFuzz (some packages required it). For instance, in Ubuntu/Debian:

    # apt-get install zlib1g-dev libgmp-dev

After [installing stack](http://docs.haskellstack.org/en/stable/README/#how-to-install), you should:

    $ git clone https://github.com/CIFASIS/QuickFuzz
    $ stack setup
    
Because *QuickFuzz* generate a lot of dependencies that may not be necessary to test an specific category of files, we slitted the project with different activation flags. Currently we have 6 flags:
    
* imgs
* archs
* docs
* codes
* media
* pki

For instance, to compile only with image generation (Bmp, Gif, Png, Ico, ..): 

    $ stack install --flag QuickFuzz:imgs

### Cabal Installation

Direct cabal installation is **not** recommended nor supported. 
