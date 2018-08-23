#! /bin/bash

HASH=`git rev-parse --short HEAD`

# Build game
elm make src/SweetBuns.elm --output build/SweetBuns-$HASH.html 
elm make src/SweetBuns.elm --debug --output build/SweetBuns-$HASH-debug.html 

# Copy media
cp -v src/*.png build/
