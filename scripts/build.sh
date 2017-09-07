#!/bin/bash

echo "* Start of build process."

gem install sass

echo "* Installed SASS."

stack setup || exit 1
stack build || exit 1

echo "* Compiled Haskell build."

git clone https://github.com/wei2912/blog _site/ || exit 1
stack exec blog-src build || exit 1

echo "* Built site."
ls -R _site/
