#!/bin/bash
export ERL_CRASH_DUMP=/dev/null
erl -noshell -s fibonacci start -s erlang halt
