#!/bin/bash

# Exit when any command fails
set -e

source functions.sh
setup_dhall_bin

# `find` like one below is not an option because it always returns exit code 0, even in case of dhall failure
# find dhall -mindepth 1 -maxdepth 1 -name "*.dhall" -print -exec bash -c "dhall text <<< './'{} > {}.html" \;

for f in dhall/*.dhall
do 
    # https://stackoverflow.com/a/965072/429311
    filename=$(basename -- "$f")
    extension="${filename##*.}"
    filename="${filename%.*}"

    echo "Generate html from $f"
    $DHALL_BIN text <<< "./$f" > "./site/$filename.html"
done
