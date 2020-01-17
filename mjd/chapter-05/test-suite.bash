#!/usr/bin/env bash

set -eu -o pipefail

i=0
for t in jsons/*.json; do
  n=${t#*/}
  n=${n%.json}

  i=$((i + 1))

  rc=0
  output=$("./json-main" "$t" 2>&1) || rc=1

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
      if [[ $n == y_* ]]; then
        # test equality between parse and output
        OUT1=$(./yajp7 "$t" | jq)
        OUT2=$(cat "$t" | jq)
        if [[ "$OUT1" == "$OUT2" ]]; then
          i=$((i + 1))
          echo "ok $i - eq for $n"
        else
          i=$((i + 1))
          echo "not ok $i - eq for $n"
        fi
      fi
    else
      echo "not ok $i - $n"
    fi
  fi
done

echo "1..$i"
