#!/bin/sh
mkdir build
elm make --optimize src/Main.elm --output=build/main.js
cp index.html build/
cp style.css build/
cp -R assets/* build/
