#!/bin/bash

echo "* Start of install process."

mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 '*/stack'

echo "** Installed Stack."

curl -L https://github.com/sass/dart-sass/releases/download/1.14.2/dart-sass-1.14.2-linux-x64.tar.gz | tar xz --wildcards --strip-components=1 '*/sass'

echo "** Installed dart-sass."

echo "* Installed all dependencies."
