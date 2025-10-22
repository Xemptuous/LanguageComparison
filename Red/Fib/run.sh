#!/bin/bash
red $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/main.red
