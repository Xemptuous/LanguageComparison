#!/bin/bash
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

cd $scriptDir

run () { $scriptDir/zig-out/bin/lexer; }
compile () { zig build && run;}

[ -f $scriptDir/zig-out/bin/lexer ] && run || compile
