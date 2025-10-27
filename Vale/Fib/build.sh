#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && valec build mymod=main.vale --output-dir target
