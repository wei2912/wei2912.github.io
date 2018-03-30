#!/bin/bash

echo "* Start of build process."

sudo apt-get update
sudo apt-get install rubygems build-essential || exit 1
curl -sSL https://get.haskellstack.org/ | sh || exit 1
gem install sass || exit 1

echo "* Installed dependencies."

