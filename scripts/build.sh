#!/bin/bash

stack --no-terminal --skip-ghc-check build || exit 1

git clone https://github.com/wei2912/blog _site/ || exit 1
stack exec blog-src build || exit 1
