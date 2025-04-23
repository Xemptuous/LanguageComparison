#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
binDir=$scriptDir/bin/Debug/net8.0

run() { $binDir/Lexer; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $binDir/Lexer ] && run || compileAndRun
