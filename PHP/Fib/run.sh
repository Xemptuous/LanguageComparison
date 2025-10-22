#!/bin/bash
php $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/main.php
