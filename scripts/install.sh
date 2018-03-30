#!/bin/bash

echo "* Start of build process."

sudo apt-get install rubygems build-essential
curl -sSL https://get.haskellstack.org/ | sh
gem install sass
stack setup
stack install

echo "* Installed dependencies."

