#!/bin/bash
wren $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/main.wren
