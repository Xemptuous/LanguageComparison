#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
# cd $scriptDir && mvn exec:java
binFile=$scriptDir/target/Java-1.0-SNAPSHOT.jar

run() { cd $scriptDir && java -cp $binFile com.code.lexer.App; }
compileAndRun() { $scriptDir/compile.sh && run; }

[ -f $binFile ] && run || compileAndRun
