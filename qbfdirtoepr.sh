#!/bin/bash
for filepath in "$1"/*.qdimacs; do
  namewithtype=${filepath##*/}
  name=${namewithtype%.*}
  echo -e "\nStarting $name"
  echo "Converting QBF to EPR using qbftoepr"
  time ./qbftoepr -i "$filepath" -o "$2"/"$name.p" -d "$3" --debug > "$4"/"$name.log"
done
