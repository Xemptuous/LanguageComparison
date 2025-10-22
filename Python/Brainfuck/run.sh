#!/bin/bash
python $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/main.py
