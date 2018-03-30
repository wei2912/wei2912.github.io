#!/bin/bash

echo "* Continuing build process from installed dependencies."

stack build || exit 1

echo "* Compiled Haskell build."

git clone https://github.com/wei2912/blog _site/ || exit 1
stack exec blog-src build || exit 1

echo "* Built site."
ls -R _site/
