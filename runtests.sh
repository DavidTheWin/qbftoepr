#!/bin/bash
mkdir -p "test_reports"
tests=("validationtests" "skolemizationtests" "removingfunctionstests" "folconversiontests" "lexandparsetests" "dependencyschemetests")
for testname in ${tests[@]}; do
  ./$testname > test_reports/"$testname"_report
  echo "$testname done"
  grep "Cases: \|Tried: \|Errors: \|Failures: " test_reports/"$testname"_report
done
