#!/bin/bash

if [ "$TRAVIS_PULL_REQUEST" != "false" ];
then
  echo "* Skipping deploy for pull request"
  exit 0
fi

echo "* Start of deploy process"

echo "* Decrypting SSH key..."
openssl aes-256-cbc -K $encrypted_68787bd95177_key -iv $encrypted_68787bd95177_iv -in travis_rsa.enc -out travis_rsa -d
chmod 700 travis_rsa
cp travis_rsa ~/.ssh/id_rsa

cd _site/
git config user.email "weien1292@gmail.com"
git config user.name "Wei En"
git config --global push.default simple
git remote set-url origin git@github.com:wei2912/wei2912.github.io || exit 1
git commit -a -m "update at $(date)" || exit 1
git push || exit 1
cd ..

echo "* Deployed site."
