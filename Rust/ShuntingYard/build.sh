#!/bin/bash
dir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
cd $dir && cargo build --release
