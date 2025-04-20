#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
[ -f $scriptDir/Lexer.out ] && $scriptDir/Lexer.out || odin run . -debug -out:$scriptDir/Lexer.out
