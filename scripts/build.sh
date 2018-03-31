#!/bin/bash

echo "* Start of build process."

stack --no-terminal --skip-ghc-check build

echo "* Compiled binaries."

git clone https://github.com/wei2912/blog _site/ || exit 1
stack exec blog-src rebuild || exit 1

echo "* Built site."
ls -R _site/
