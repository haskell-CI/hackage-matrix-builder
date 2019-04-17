# Build instructions

## Pre-requisites

- Cabal 2.4.1 or newer (e.g. from https://launchpad.net/~hvr/+archive/ubuntu/ghc)
- GHCJS 8.4 (e.g. from https://launchpad.net/~hvr/+archive/ubuntu/ghcjs)
- A recent version of `happy` needs to be in `$PATH` during the initial invocation of `cabal build` (mostly for `haskell-src-exts`)

### Optional additional tools used by `Makefile`

- `cabal-plan` (e.g. via `cabal v2-install cabal-plan`)
- `ccjs` (https://www.npmjs.com/package/closurecompiler)

## Build

### Manually

Simply

    cabal v2-build

The resulting compiled JavaScript module will be at

    ./*/build/*/ghcjs-8.4*/matrix-ui-*/x/matrix-ui/build/matrix-ui/matrix-ui.jsexe/all.js

### Via `Makefile`

Use

    make all

which will build and update the top-level `ui.v3` folder which is served by the matrix-webserver.

There's also `make all-min` which will additionally minimize the JS code via `ccjs`. There's also `make clean` to remove the temporary build files.
