#!/bin/bash
node $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/main.js
