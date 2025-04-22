#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
outFile=$scriptDir/main.out

compile() { g++ $scriptDir/*.cpp -o $outFile; $outFile; }

[ -f $outFile ] && $outFile || compile && $outFile;
