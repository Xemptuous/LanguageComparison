#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && fennel --compile main.fnl > fib.lua && luac -o fib.out fib.lua
