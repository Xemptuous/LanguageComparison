#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && R -q -e 'library(compiler)' -e 'cmpfile("main.R", "main.Rc")'
