#!/bin/bash
rakudo $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/main.raku
