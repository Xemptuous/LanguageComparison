#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
binDir=$scriptDir/bin

cd $scriptDir && dmd *.d -of=bin/app.out -od=lib
