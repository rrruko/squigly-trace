#!/bin/bash
SAMPLES=$1
stack build --ghc-options="-Wall -O2"
stack exec squigly-trace-exe $SAMPLES
feh render/result.png
