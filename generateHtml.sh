#!/bin/bash

# Exit when any command fails
set -e

# Find like one below is not an option because it always return exit code 0, even in case of dhall text failure
# find dhall -mindepth 1 -maxdepth 1 -name "*.dhall" -print -exec bash -c "dhall text <<< './'{} > {}.html" \;

for f in dhall/*.dhall
do 
    echo "Generate html from $f"
    dhall text <<< "./$f" > "$f.html"
done

cd dhall
find . -iname '*.dhall.html' -exec sh -c 'mv "$0" "${0%.dhall.html}.html"' {} \;
find . -iname "*.html" -exec mv {} ../site/{} \;
