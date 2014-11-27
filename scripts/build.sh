#!/bin/bash

# compile binary
mkdir bin
ghc -o bin/site --make -threaded src/*.hs

# build site
./bin/site build

