#! /bin/bash

HASH=`git rev-parse --short HEAD`

# Build game
npx elm make src/SweetBuns.elm --output build/main.js --optimize
npx elm make src/SweetBuns.elm --output build/SweetBuns-$HASH.html 
npx elm make src/SweetBuns.elm --debug --output build/SweetBuns-$HASH-debug.html 


# Copy static files
cp -v src/*.html build/
cp -v src/*.css build/
cp -v src/*.png build/
