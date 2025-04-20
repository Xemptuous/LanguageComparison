#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

run () { $scriptDir/main.out; }
compile () { g++ $scriptDir/*.cpp -o $scriptDir/main.out; run; }

[ -f $scriptDir/main.out ] && run || compile
