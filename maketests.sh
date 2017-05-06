tests=("validationtests" "skolemizationtests" "removingfunctionstests" "folconversiontests" "lexandparsetests" "dependencyschemetests")
for testname in ${tests[@]}; do
  make "$testname"
done
