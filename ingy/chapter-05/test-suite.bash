#!/usr/bin/env bash

set -eu -o pipefail

i=0
for t in json-test-suite/*.json; do
  n=${t#*/}
  n=${n%.json}

  i=$((i + 1))

  rc=0
  output=$(./json-parser < "$t" 2>&1) || rc=1

  if [[ $n == i_* ]]; then
    if [[ $rc -eq 0 ]]; then
      echo "ok $i - EITHER/PASS - $n"
    else
      echo "ok $i - EITHER/FAIL - $n"
    fi
  else
    [[ $n == n_* ]] && ok=1 || ok=0
    if [[ $rc -eq $ok ]]; then
      echo "ok $i - $n"
    else
      echo "not ok $i - $n"
    fi
  fi
done

echo "1..$i"
