#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && kotlinc Main.kt -include-runtime -d Main.jar
