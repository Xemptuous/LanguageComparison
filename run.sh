#!/bin/sh

scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")

file=run.sh
compile=false
run=false
while getopts ":hcr" opt; do
  case $opt in
    h)
      echo "Usage: $0 [-h] [-c r] language project"
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
    # :)
    #   echo "Option -$OPTARG requires an argument." >&2
    #   exit 1
    #   ;;
  esac
done

shift $((OPTIND -1))

language="${1^}"
project="${2^}"

if [[ $language == "" ]]; then
    echo "Language not specified."
elif [[ $project == "" ]]; then
    echo "Project not specified."
else
    cd $scriptDir/$language/$project;
    if ! $compile && ! $run; then ./compile.sh && ./run.sh; fi
    $compile && ./compile.sh
    $run && ./run.sh
fi
