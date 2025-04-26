#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
buildDir="$scriptDir/_build"

mkdir -p $buildDir

run () { cd $buildDir && erl -noinput -eval 'main:start()' -eval 'erlang:halt()'; }
compileAndRun() { $scriptDir/compile.sh && run; }

[[ -f $buildDir/main.beam && -f $buildDir/lexer.beam ]] && run || compileAndRun
