#!/bin/sh
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

run() { node $scriptDir/build/main.js; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $scriptDir/build/main.js ] && run || compileAndRun

# npx tsx ./main.ts
