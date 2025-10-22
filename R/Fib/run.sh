#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && R -q -e 'library(compiler)' -e 'loadcmp("main.Rc")'
