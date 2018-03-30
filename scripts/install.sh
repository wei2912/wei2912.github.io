#!/bin/bash

echo "* Start of build process."

sudo apt-get update
gem install sass || exit 1
wget -qO- https://get.haskellstack.org/ | sudo || exit 1

echo "* Installed dependencies."

