#!/bin/sh
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
python $scriptDir/main.py
