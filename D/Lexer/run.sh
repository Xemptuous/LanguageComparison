#!/bin/sh

dmd ./*.d
./app
rm app app.o
