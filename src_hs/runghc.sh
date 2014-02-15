#!/bin/sh

ghc -odir ../out -hidir ../out -o ../out/Main Main && ../out/Main 2>&1 | ./pretty.sh

# profiling
# ghc -prof -fprof-auto -rtsopts -odir ../out -hidir ../out -o ../out/Main Main && ../out/Main +RTS -p 2>&1 | ./pretty.sh