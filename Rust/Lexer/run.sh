#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

run() { $scriptDir/target/debug/lexer;  }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $scriptDir/target/debug/lexer ] && run || compileAndRun
