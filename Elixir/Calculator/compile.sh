#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
buildDir="$scriptDir/_build"

mkdir -p $buildDir
cd $scriptDir && elixirc main.exs -o $buildDir
