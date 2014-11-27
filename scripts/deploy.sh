#!/bin/bash

cd _site/
git commit -a -m "update at $(date)"
git push
cd ..

