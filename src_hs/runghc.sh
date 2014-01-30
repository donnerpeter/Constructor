#!/bin/sh

ghc -odir ../out -hidir ../out -o ../out/Parser Parser && ../out/Parser 2>&1 | ./pretty.sh

# profiling
# ghc -prof -fprof-auto -rtsopts -odir ../out -hidir ../out -o ../out/Parser Parser && ../out/Parser +RTS -p 2>&1 | ./pretty.sh