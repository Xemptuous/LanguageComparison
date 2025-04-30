#!/bin/sh
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

compile=false
run=false
all=false

while getopts ":hcrat" opt; do
  case $opt in
    a)
      all=true
      ;;
    h)
      echo "Usage: $0 [-h] [-c r a t] language project"
      exit 0
      ;;
    c)
      compile=true
      ;;
    r)
      run=true
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

shift $((OPTIND -1))

TIMEFORMAT='%3R'

language="${1^}"
project="${2^}"

if $all; then
    for lang in $(find . -maxdepth 1 -noleaf -type d -name '[A-Z]*' -printf '%f\n' | sort); do
        for proj in $scriptDir/$lang/*; do
            $compile && \
                echo -e -n "${lang^} $(basename $proj) Compiling..." && \
                time $proj/compile.sh 2&>/dev/null
            $run && \
                # echo -e -n "${lang^} $(basename $proj) Running....." && \
                printf "%-12s%-15sRunning....." "$lang" "$(basename $proj)" | sed 's/ /./g'
                time $proj/run.sh 2&>/dev/null
        done
    done
else
    if [[ $language == "" ]]; then
        echo "Language not specified."
    elif [[ $project == "" ]]; then
        echo "Project not specified."
    else
        cd $scriptDir/$language/$project;
        $compile && \
            echo -e -n "${language^} ${project^} Compiling..." && \
            time ./compile.sh 2&>/dev/null
        $run && \
            echo -e -n "${language^} ${project^} Running....." && \
            time ./run.sh 2&>/dev/null
    fi
fi
