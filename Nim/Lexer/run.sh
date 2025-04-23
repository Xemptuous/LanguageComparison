#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

run () { cd $scriptDir && $scriptDir/main.out; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $scriptDir/main.out ] && run || compileAndRun
