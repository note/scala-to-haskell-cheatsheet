#!/bin/bash

# This script verifies whether html files generated out from dhall do not differ from ones in `site` directory
# In other way it will not allow to manually edit html files but they're still as part of repository which
# is useful because you can track how HTML changes over time. In particular it gives you ability to 
# distinguish dhall refactoring, which does not changes html, from other changes

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
    $DHALL_BIN text <<< "./$f" > "./dhall/$filename.html"
    echo "Verify html: ./dhall/$filename.html vs ./site/$filename.html"
    diff "./dhall/$filename.html" "./site/$filename.html"

    rm "./dhall/$filename.html"
done
