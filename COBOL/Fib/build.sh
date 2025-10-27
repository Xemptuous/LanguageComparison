#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && cobc -x -free -O3 -fstatic-call -o fib.out Fibonacci.cob
