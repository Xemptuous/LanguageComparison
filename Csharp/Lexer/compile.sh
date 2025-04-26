#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $scriptDir && dotnet build --nologo --no-dependencies --no-restore -v q
