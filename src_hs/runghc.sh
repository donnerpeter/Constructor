#!/bin/sh

runghc "$@" 2>&1 | ./pretty.sh