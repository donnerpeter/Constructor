#!/bin/sh

ghci "$@" 2>&1 | ./pretty.sh