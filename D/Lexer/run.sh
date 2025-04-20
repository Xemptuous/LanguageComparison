#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
binDir=$scriptDir/bin

run () { $bindir/app.out; }
compile () { dmd *.d -of=bin/app.out -od=lib && run; }

[ -f $binDir/app.out ] && run || compile
