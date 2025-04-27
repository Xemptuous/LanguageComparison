#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
binDir=$scriptDir/target/lexer-0.1.0-SNAPSHOT-standalone.jar
# cd $sciptDir && clojure -M -m lexer.core
cd $sciptDir && java -cp $binDir lexer.core
