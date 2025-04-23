#!/bin/bash
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
buildDir=$scriptDir/build
binDir=$buildDir/bin

mkdir -p $binDir
mkdir -p $buildDir

run () { $binDir/main.out; }
compileAndRun() { $scriptDir/compile.sh && run; }

[[ -f $buildDir/Main.hi && -f $buildDir/Main.o ]] && run || compileAndRun

