#!/bin/bash

echo "* Start of build process."

# compile binary
mkdir bin
ghc -o bin/site --make -O -threaded src/*.hs

echo "* Compiled binaries."

# build site
git clone https://github.com/wei2912/wei2912.github.io _site/
bin/site build

echo "* Built site."
