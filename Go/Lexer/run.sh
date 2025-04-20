#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

run () { $scriptDir/main.out; }
compile () { go build -C $scriptDir -o main.out && run; }

[ -f $scriptDir/main.out ] && run || compile
