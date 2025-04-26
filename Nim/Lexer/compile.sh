#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $scriptDir && nim c -o:main.out --threads:on --verbosity:0 --parallelBuild:0 main.nim
