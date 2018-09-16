#!/bin/bash

if [ "$TRAVIS_PULL_REQUEST" != "false" ];
then
  echo "* Skipping deploy for pull request"
  exit 0
fi

echo "* Start of deploy process"

cd _site/
git config user.email "weien1292@gmail.com"
git config user.name "Ng Wei En"
git config --global push.default simple
git remote set-url origin git@github.com:wei2912/blog || exit 1
git add -A && git commit --allow-empty -m "update at $(date)" || exit 1
git push || exit 1
cd ..

echo "* Deployed site."
