#!/bin/bash
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

ghc -dynamic Main.hs &>/dev/null
$scriptDir/Main
rm $scriptDir/*{.o,.hi} 2&>/dev/null
