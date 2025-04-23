#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $scriptDir && tsc -outDir $scriptDir/build $scriptDir/*.ts
