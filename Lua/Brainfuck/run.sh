#!/bin/bash
lua $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/luac.out
