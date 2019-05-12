CCJS=npx google-closure-compiler

.PHONY: all
all:
	cabal new-build exe:matrix-ui
	rm -fv ../ui.v3/ui.js
	cp -sv $$(cabal-plan list-bin matrix-ui:exe:matrix-ui).jsexe/all.js ../ui.v3/ui.js

.PHONY: all-min
all-min:
	cabal new-build exe:matrix-ui

	rm -fv ../ui.v3/ui.js
	D=$$(cabal-plan list-bin matrix-ui:exe:matrix-ui).jsexe; $(CCJS) $$D/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=$$D/all.js.externs > ../ui.v3/ui.js

.PHONY: clean
clean:
	rm -rf dist-newstyle dist
