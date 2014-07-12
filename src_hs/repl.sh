#!/bin/sh

cabal repl "$@" 2>&1 | ./pretty.sh