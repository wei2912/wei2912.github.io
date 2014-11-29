#!/bin/bash

echo "* Start of build process."

# compile binary
mkdir bin
ghc -o bin/site --make -O -threaded src/*.hs || exit 1

echo "* Compiled binaries."

# build site
git clone https://github.com/wei2912/wei2912.github.io _site/ || exit 1
bin/site build || exit 1

echo "* Built site."
