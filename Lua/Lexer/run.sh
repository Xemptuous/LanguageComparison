#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
lua $scriptDir/main.lua

run() { cd $scriptDir && lua $scriptDir/luac.out; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $scriptDir/luac.out ] && run || compileAndRun
