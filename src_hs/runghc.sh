#!/bin/sh

# PROFILE=true

if [ $PROFILE ]; then
  echo profiling
  cabal configure --enable-executable-profiling
  cabal build profiled && time -f "elapsed %E" dist/build/profiled/profiled +RTS -p 2>&1 | ./pretty.sh
else
  cabal configure --disable-executable-profiling
  cabal build constructor && time -f "elapsed %E" dist/build/constructor/constructor 2>&1 | ./pretty.sh
fi