#!/bin/bash
tests="exp-true 
       exp-false 
       fact-true
       tak-false"

for test in ${tests}; do
  echo "Running ${test}"
  cem=0; ghc=0; ratio=0;
  for i in {1..5}; do
    cem=$(echo $cem +  `{ time -p ./cem ./test/${test}.lc > /dev/null; } 2>&1 | grep user | awk '{print $2}'` | bc)
    ghc=$(echo $ghc + `{ time -p ./test/hs/${test} > /dev/null; } 2>&1 | grep user | awk '{print $2}'` | bc)
  done;
  cem=$(echo "scale=3; $cem/5" | bc)
  echo cem took average of $cem over 5 runs
  ghc=$(echo "scale=3; $ghc/5" | bc)
  echo ghc took average of $ghc over 5 runs
  ratio=`echo "scale=3; $cem/$ghc" | bc`
  echo "cem takes $ratio the time"
done;
      

