#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && nim c -o:main.out --threads:on --verbosity:0 --parallelBuild:0  main.nim
