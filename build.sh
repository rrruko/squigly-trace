#!/bin/bash
stack build --ghc-options="-Wall -O2"
stack exec squigly-trace-exe
feh render/result.png
