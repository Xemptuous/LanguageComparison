#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && gnatmake main -o main.out
