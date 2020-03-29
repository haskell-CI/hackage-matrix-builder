#!/bin/bash -e

mkdir -p worker/bin

# cabal executable
if [ ! -x worker/bin/cabal ]; then
  echo "missing cabal executable"
  exit 1
fi

# worker
cabal v2-build exe:matrix-worker
cp -v "$(cabal new-exec which -- matrix-worker)" ./worker/bin/
strip -v worker/bin/matrix-worker

# 
docker build -t matrix-worker-base ./worker-base
docker build -t matrix-worker      ./worker

