#!/bin/bash
tclsh $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/main.tcl
