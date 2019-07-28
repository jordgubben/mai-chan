#!/bin/bash

echo "~Test game~"
npx elm-test

echo "~Test 'game-board' package~" 
cd deps/game-board/
npx elm-test
cd -
