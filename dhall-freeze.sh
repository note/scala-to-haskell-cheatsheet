#!/bin/bash

# Exit on any failure
set -e

source functions.sh
setup_dhall_bin

# https://stackoverflow.com/a/33374642/429311
shopt -s globstar

for f in dhall/**/*.dhall
do
    echo "dhall freeze: $f"
    cat $f | dhall --ascii freeze > "$f.frozen"
    mv "$f.frozen" $f
done
