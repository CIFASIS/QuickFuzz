# QuickFuzz

An experimental grammar fuzzer in Haskell using QuickCheck, Template Haskell and specific libraries from Hackage.
It has found [some interesting bugs](http://QuickFuzz.org/).

## Authors

* Pablo **Buiras** ([Chalmers University of Technology](http://www.chalmers.se/en/Pages/default.aspx))
* Martín **Ceresa** ([CIFASIS-Conicet](http://cifasis-conicet.gov.ar/))
* Gustavo **Grieco** ([CIFASIS-Conicet](http://cifasis-conicet.gov.ar/) and [VERIMAG](http://www-verimag.imag.fr/?lang=en))

## Students

* Franco Constantini

## Former Members

* Martín **Escarrá** ([Universidad Nacional de Rosario](http://www.unr.edu.ar/))

## Installation

### Stack
Because *QuickFuzz* generate a lot of dependencies that may not be necessary to
test an specific category of files, we slitted the project with different
activation flags. Currently we have 5 flags:
    
* imgs
* archs
* docs
* codes
* media

We **recommend** and stimulate the use (and abuse) of
[Stack](www.haskellstack.org).
    
    $ stack install --flags Quickfuzz:imgs

###  Manual Compilation

 * GHC 7.10
 * Cabal 1.24?
 * zlib-dev
 * libbz2-dev

In Ubuntu, a PPA to install GHC 7.10 is available [here](https://launchpad.net/~hvr/+archive/ubuntu/ghc). Do not forget to add suitable directories
to your path (e.g. /opt/ghc/7.10.3/bin and /opt/cabal/1.22/bin)

### Cabal Installation

With *Cabal* we recommend the creation of a sandbox.

    $ git clone https://github.com/CIFASIS/QuickFuzz.git
    $ cd QuickFuzz
    $ cabal sandbox init
    $ cabal install -f imgs 
