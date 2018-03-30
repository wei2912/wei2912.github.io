#!/bin/bash

echo "* Start of build process."

sudo apt-get update
sudo apt-get install ruby ruby-dev || exit 1
gem install sass || exit 1
sudo -c 'curl -sSL https://get.haskellstack.org/ | sh' || exit 1

echo "* Installed dependencies."

