#!/bin/bash

echo "* Start of clean process"

bin/site clean || exit 1
rm -rf bin _cache _site

echo "* All leftover data removed"
