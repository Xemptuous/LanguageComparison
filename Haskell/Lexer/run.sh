#!/bin/bash
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

ghc -dynamic Main.hs
$scriptDir/Main
rm $scriptDir/*{.o,.hi} Main 2&>/dev/null
