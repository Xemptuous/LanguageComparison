#!/bin/bash

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
binDir=$scriptDir/bin/Debug/net8.0

run () { $binDir/Lexer; }
compile () { dotnet build && run; }

[ -f $binDir/Lexer ] && run || compile
