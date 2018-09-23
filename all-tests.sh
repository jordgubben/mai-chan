#!/bin/bash

echo "~Test game~"
elm-test

echo "~Test 'game-board' package~" 
cd deps/game-board/
elm-test
cd -
