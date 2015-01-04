# Hackage GHC version / package version build-matrix builder

## Usage

Currently the set of GHC executables is hardcoded, tweak
`ghcExecutables` in `src/hackage-matrix-builder.hs` as needed.

Here's simple example session:

    cabal run -v0 hackage-matrix-builder deepseq | tee deepseq.log

    cabal run -v0 hackage-matrix-html-report < deepseq.log > deepseq.html

