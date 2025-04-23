#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $scriptDir && odin build . -debug -out:$scriptDir/Lexer.out
