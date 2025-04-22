#!/bin/bash
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
buildDir=$scriptDir/build
binDir=$buildDir/bin

mkdir -p $binDir

run () { $binDir/main.out; }
compile () { ghc -dynamic -o $binDir/main.out -odir $buildDir -hidir $buildDir Main.hs && run; }

[[ -f $buildDir/Main.hi && -f $buildDir/Main.o ]] && run || compile

