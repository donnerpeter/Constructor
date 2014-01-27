#!/bin/sh

ghc -odir ../out -hidir ../out -o ../out/Parser Parser && ../out/Parser 2>&1 | ./pretty.sh