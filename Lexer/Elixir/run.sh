#!/bin/sh
elixirc main.exs
rm ./*.beam 2&>/dev/null
