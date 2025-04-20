#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
buildDir="$scriptDir/_build"

mkdir -p $buildDir

run () { cd $buildDir; elixir -e 'Main.main'; }
compile () { elixirc main.exs -o $buildDir && run; }

[ -f $buildDir/Elixir.Main.beam ] && run || compile
