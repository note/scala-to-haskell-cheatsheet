#!/bin/bash

# Exit when any command fails
set -e

if which dhall; then
    DHALL_BIN=`which dhall`
    echo "Using dhall: $DHALL_BIN"
else
    echo "No installation of dhall detected - downloading 1.29.0..."
    wget https://github.com/dhall-lang/dhall-haskell/releases/download/1.29.0/dhall-1.29.0-x86_64-linux.tar.bz2
    tar xjf dhall-1.29.0-x86_64-linux.tar.bz2
    DHALL_BIN="$PWD/bin/dhall"
    echo "Using downloaded dhall: $DHALL_BIN"
fi

$DHALL_BIN --version

# Find like one below is not an option because it always return exit code 0, even in case of dhall text failure
# find dhall -mindepth 1 -maxdepth 1 -name "*.dhall" -print -exec bash -c "dhall text <<< './'{} > {}.html" \;

for f in dhall/*.dhall
do 
    echo "Generate html from $f"
    $DHALL_BIN text <<< "./$f" > "$f.html"
done

cd dhall
find . -iname '*.dhall.html' -exec sh -c 'mv "$0" "${0%.dhall.html}.html"' {} \;
find . -iname "*.html" -exec mv {} ../site/{} \;
