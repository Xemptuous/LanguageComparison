#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
binDir=$scriptDir/_build/default/bin
cd $scriptDir

# run() { dune exec Lexer; }
run() { $binDir/main.exe;  }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $binDir/main.exe ] && run || compileAndRun
