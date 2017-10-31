#!/bin/bash
SAMPLES=$1
if stack build --ghc-options="-Wall -O2"; then
    stack exec squigly-trace -- -s $SAMPLES
    feh render/result.png
fi
