#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
pp -o $dir/fib.out $dir/fib.pl
