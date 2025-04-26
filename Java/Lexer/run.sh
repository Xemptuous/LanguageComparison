#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
binFile=$scriptDir/target/Java-1.0-SNAPSHOT.jar

run() { cd $scriptDir && java -cp $binFile com.code.lexer.App; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $binFile ] && run || compileAndRun

# cd $scriptDir && mvn exec:java
