#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && koka -O3 -o fib.out main.kk && chmod +x fib.out
