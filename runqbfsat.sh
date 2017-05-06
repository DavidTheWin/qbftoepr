#!/bin/bash
function contains_element {
  local e
  for e in "${@:2}"; do [[ "$e" == "$1" ]] && return 0; done
  return 1
}

function check_for_errors {
  errors=$(echo $1 | grep "Fatal")
  if [ -n "${errors}" ]; then
    echo "Error from qbftoepr: ${errors}"
    exit
  fi
}

args=($1 $2 $3 $4 $5 $6)
if contains_element "--help" "${args[@]}" || [ -z "$1" ]; then
  echo "Arguments given to the script are passed through to qbftoepr with the exception that if --debug is given without -o being specified the script will exit\n Arguments must be given in the order '-i input -o output --debug' or '--help'"
  exit
fi

if ! contains_element "-i" "${args[@]}" ; then
  echo "An input file is required as the first argument: -i filename"
  exit
elif [ ! "$1" = "-i" ]; then
  echo "The first argument must be an input file: -i filename"
  exit
fi

if [ "$3" = "--debug" ] || [ "$4" = "--debug" ]; then
  echo "When using the debug option an output file must be given first"
  exit
fi

if [ "$3" = "-o" ]; then
  output=$(./qbftoepr $1 $2 $3 $4 $5 2>&1| tee)
  check_for_errors ${output}
  echo ${output}
  ../../iproveropt $4
else
  output=$(./qbftoepr $1 $2 2>&1| tee)
  check_for_errors ${output}
  echo ${output}
  echo ${output} | ../../iproveropt --stdin true
fi
