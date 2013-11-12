#!/bin/bash

# After installing the Haskell Platform, run this script to grab the packages that the MUD needs.

cabal update
cabal install cabal-install
cabal install lens
cabal install text

brew install readline
cabal install readline --extra-include-dirs=/usr/local/Cellar/readline/6.2.4/include/ --extra-lib-dirs=/usr/local/Cellar/readline/6.2.4/lib/ --configure-option=--with-readline-includes=/usr/local/Cellar/readline/6.2.4/include/ --configure-option=--with-readline-libraries=/usr/local/Cellar/readline/6.2.4/lib/
