#!/bin/sh
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

run () { node $scriptDir/build/main.js; }
compile () { tsc -outDir $scriptDir/build $scriptDir/*.ts && run; }

[ -f $scriptDir/build/main.js ] && run || compile

# npx tsx ./main.ts
