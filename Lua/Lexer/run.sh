#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

# to build
#   luac $scriptDir/main.lua
#   lua $scriptDir/luac.out

lua $scriptDir/main.lua
