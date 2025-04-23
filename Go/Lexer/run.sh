#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

run () { $scriptDir/main.out; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $scriptDir/main.out ] && run || compileAndRun
