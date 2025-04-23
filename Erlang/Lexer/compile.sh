#!/bin/sh

# order of operations:
# erl
# c(main).
# c(token).
# main:start().

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
buildDir="$scriptDir/_build"

mkdir -p $buildDir

cd $scriptDir && erlc -o $buildDir $scriptDir/*.erl
