#!/bin/bash

echo "* Start of clean process"

bin/site clean
rm -rf bin _cache _site

echo "* All leftover data removed"
