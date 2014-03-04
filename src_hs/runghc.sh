#!/bin/sh

ghc -O2 -odir ../out -hidir ../out -o ../out/Main Main && time -f "elapsed %E" ../out/Main 2>&1 | ./pretty.sh

# profiling
# ghc -O2 -prof -fprof-auto -rtsopts -odir ../out -hidir ../out -o ../out/Main Main && ../out/Main +RTS -p 2>&1 | ./pretty.sh