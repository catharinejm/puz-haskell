#!/usr/bin/env bash

set -e

echo ================================================================================
echo Building...
echo

stack build

echo Done!
echo ================================================================================
echo

rlwrap stack exec puz-exe "$@"
