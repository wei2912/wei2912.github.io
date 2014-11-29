#!/bin/bash

echo "* Start of deploy process"

cd _site/
git config user.email "weien1292@gmail.com"
git config user.name "Wei En"
git config --global push.default simple
git remote set-url origin git@github.com:wei2912/wei2912.github.io
git commit -a -m "update at $(date)"
git push
cd ..

echo "* Deployed site."
