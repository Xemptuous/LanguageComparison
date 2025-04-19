#!/bin/sh

# order of operations:
# erl
# c(main).
# c(token).
# main:start().

erlc *.erl
erl -noinput -eval 'main:start()' -eval 'init:stop()'
rm ./*.beam
