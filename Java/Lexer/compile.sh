#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
srcDir=$scriptDir/src/main/java/com/code/lexer
targetDir=$scriptDir/target/classes

javac -nowarn $srcDir/*.java -d $targetDir/

# mvn package -Dmaven.test.skip -DskipTests -o -q
