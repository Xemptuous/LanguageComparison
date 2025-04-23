#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
outFile=$scriptDir/main.out

run() { $outFile; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $outFile ] && run || compileAndRun;
