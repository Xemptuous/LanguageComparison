#!/bin/sh
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

cd $scriptDir && python -O -m compileall $scriptDir/ 2&>/dev/null
