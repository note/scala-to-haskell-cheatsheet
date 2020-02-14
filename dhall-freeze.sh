#!/bin/bash

# Exit on any failure
set -e

source functions.sh
setup_dhall_bin

# https://stackoverflow.com/a/33374642/429311
shopt -s globstar

find . -iname "*.dhall" -exec dhall freeze --inplace {} --ascii \;

