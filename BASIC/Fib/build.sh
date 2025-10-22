#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && fbc -O 3 -x main.out main.bas
