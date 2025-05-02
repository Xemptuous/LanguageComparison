#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
buildDir="$scriptDir/_build"

mkdir -p $buildDir

run () { cd $buildDir; elixir -e 'Main.main'; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $buildDir/Elixir.Main.beam ] && run || compileAndRun
