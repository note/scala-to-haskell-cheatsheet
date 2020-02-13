#!/bin/bash

setup_dhall_bin () {
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
}
