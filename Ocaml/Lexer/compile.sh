#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

cd $scriptDir && dune build
