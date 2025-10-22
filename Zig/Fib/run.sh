#!/bin/bash
$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/zig-out/bin/Zig
