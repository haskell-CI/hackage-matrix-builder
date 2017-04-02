#!/bin/bash
# If this fails you probably need to `npm install -g purescript-psa`.
pulp browserify --stash --censor-lib --censor-codes=ImplicitImport,UnusedImport,UnusedExplicitImport > ../ui/purescript.js
