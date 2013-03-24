#!/bin/bash
tests="exp-true 
       exp-false 
       fact-true
       tak-true"


for test in ${tests}; do
  echo "Running ${test}"
  cem=`{ time -p ./cem ./test/${test}.lc > /dev/null; } 2>&1 | grep real | awk '{print $2}'`
  echo "cem took $cem seconds"
  ghc=`{ time -p ./test/hs/${test} > /dev/null; } 2>&1 | grep real | awk '{print $2}'`
  echo "ghc took $ghc seconds" 
  ratio=`echo "scale=2; $cem/$ghc" | bc`
  echo "cem takes $ratio the time"

done;
      

