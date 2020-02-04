#!/bin/bash

find dhall -mindepth 1 -maxdepth 1 -name "*.dhall" -print -exec bash -c "dhall text <<< './'{} > {}.html" \;
cd dhall
find . -iname '*.dhall.html' -exec sh -c 'mv "$0" "${0%.dhall.html}.html"' {} \;
find . -iname "*.html" -exec mv {} ../{} \;
