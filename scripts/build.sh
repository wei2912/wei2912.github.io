#!/bin/bash

# compile binary
mkdir bin
ghc -o bin/site --make -O -threaded src/*.hs

# build site
./bin/site build

