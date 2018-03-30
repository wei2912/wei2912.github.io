#!/bin/bash

echo "* Start of build process."

sudo apt-get update
gem install sass || exit 1
sudo 'wget -qO- https://get.haskellstack.org/ | sh' || exit 1

echo "* Installed dependencies."

