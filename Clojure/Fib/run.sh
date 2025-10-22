#!/bin/bash
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
# cd $scriptDir/target/uberjar && java -cp fib-0.1.0-SNAPSHOT-standalone.jar fib.core
exec java -jar "$scriptDir/target/uberjar/fib-0.1.0-SNAPSHOT-standalone.jar"
