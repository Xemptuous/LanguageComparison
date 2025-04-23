#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

run() { $scriptDir/Lexer.out; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $scriptDir/Lexer.out ] && run || compileAndRun
