#!/bin/bash -e

mkdir -p bin

# cabal executable
if [ ! -x bin/cabal ]; then
  echo "missing cabal executable"
  exit 1
fi

# worker
cabal v2-build exe:matrix-worker
cp -v "$(cabal new-exec which -- matrix-worker)" bin/
strip -v bin/matrix-worker

# 
docker build -t matrix-worker-base -f base.Dockerfile .
docker build -t matrix-worker .

