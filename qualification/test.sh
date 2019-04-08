#!/bin/bash

set -eu

stack exec -- ghc +RTS -V0 -RTS -rtsopts -O2 -o Solution "$1.hs"

./Solution < "$1.txt"

cat "$1.hs" | xclip -selection c
