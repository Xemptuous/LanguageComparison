#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
binDir=$scriptDir/bin

run() { $binDir/app.out; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $binDir/app.out ] && run || compileAndRun
