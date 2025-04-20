#!/bin/sh

# order of operations:
# erl
# c(main).
# c(token).
# main:start().

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
buildDir="$scriptDir/_build"

mkdir -p $buildDir


run () { cd $buildDir && erl -noinput -eval 'main:start()' -eval 'init:stop()'; }
compile () { erlc -o $buildDir $scriptDir/*.erl && run; }

[[ -f $buildDir/main.beam && -f $buildDir/lexer.beam ]] && run || compile
