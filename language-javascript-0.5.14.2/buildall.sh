#!/bin/sh

# First run alex on Lexer.x to generate Lexer.hs
#./runalex.sh

# do a clean build of all, including the tests
#cabal clean && cabal configure -fbuildtests && cabal build && cabal haddock
cabal clean && cabal configure --enable-tests && cabal build && cabal test && cabal haddock
