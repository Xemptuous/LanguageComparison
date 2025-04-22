#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
outFile=$scriptDir/main.out

g++ $scriptDir/*.cpp -o $outFile; $outFile
