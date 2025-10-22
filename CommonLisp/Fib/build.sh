#!/usr/bin/env bash
set -euo pipefail
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
# Compile to a standalone executable named 'fib'
sbcl --noinform --disable-debugger \
     --load "$scriptDir/main.lisp" \
     --eval '(sb-ext:save-lisp-and-die "fib.out" :executable t :toplevel #'\''main :compression 0)'
