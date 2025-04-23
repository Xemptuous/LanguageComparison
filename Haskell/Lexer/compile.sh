#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
buildDir=$scriptDir/build
binDir=$buildDir/bin

mkdir -p $binDir
ghc -dynamic -o $binDir/main.out -odir $buildDir -hidir $buildDir $scriptDir/*.hs
