#!/bin/bash

echo "* Start of install process"

cabal install hakyll || exit 1

echo "** Installed Hakyll"

gem install sass

echo "** Installed SASS"

echo "* Installed dependencies."
