#!/bin/bash

for DIR in p*; do
  cd "$DIR" || exit 1
  cargo test
  rc=$?
  cd .. || exit 1

  [[ $rc -eq 0 ]] || exit 1
  echo "*** Test $DIR Passed! ***"
  echo ""
done

