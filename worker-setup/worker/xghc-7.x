#!/bin/bash

DN=$(dirname "$0")
BN0=$(basename "$0")

BN=${BN0/#xghc/ghc}

if [ "$BN0" = "$BN" ]; then
  echo "xghc: internal error" 1>&2
  exit 42
fi

GHCBIN="$DN/$BN"

if [ ! -x "$GHCBIN" ]; then
  echo "xghc: $GHCBIN not found" 1>&2
  exit 42
fi

is_make=false

for arg in "$@"; do if [ "$arg" = "--make" ];then is_make=true; break; fi; done

if $is_make; then
    exec timeout --kill-after=15 1500 "$GHCBIN" +RTS -M1750M -t -RTS  "$@"
fi

# kill after 25 minutes; kill hard 15s later
exec timeout --kill-after=15 1500 "$GHCBIN" +RTS -M1750M -RTS  "$@"
