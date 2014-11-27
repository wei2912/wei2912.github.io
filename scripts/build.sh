#!/bin/bash

# compile binary
mkdir bin
ghc -o bin/site --make -O -threaded src/*.hs

# build site
git clone https://github.com/wei2912/wei2912.github.io _site/
bin/site build
