#!/bin/bash
ruby $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/main.rb
