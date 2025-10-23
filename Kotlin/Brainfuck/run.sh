#!/bin/bash
java -jar $(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/Main.jar
