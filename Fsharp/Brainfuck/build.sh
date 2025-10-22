#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && dotnet build --nologo --no-dependencies --no-restore -v q
