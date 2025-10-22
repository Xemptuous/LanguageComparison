#!/bin/bash
julia $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/main.jl
