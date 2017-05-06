#!/bin/bash
for filepath in "$1"/*.qdimacs; do
  namewithtype=${filepath##*/}
  name=${namewithtype%.*}
  echo -e "\nStarting $name"
  echo "Converting QBF to EPR using qbftoepr"
  time ./qbftoepr -i "$filepath" -o "$2"/"$name.p" -d "$3" --debug > "$4"/"$name.log"
  echo "Solving with iProver"
  time ../../iproveropt "$2"/"$name.p" > "$5"/"$name.iproverout"
  grep "SZS status" "$5"/"$name.iproverout"
done
