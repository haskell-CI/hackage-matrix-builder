# Convenience Makefile

.PHONY: all clean ui exe run-ui
all: ui exe

exe:
	cabal new-build :pkg:hackage-matrix-builder3

ui:
	cd src-ui && npm run build

clean:
	rm -f ui/ui.js
#	cd src-ui && npm clean
	rm -rf src-ui/output
	rm -rf dist-newstyle

run-ui: ui
	cabal new-run hackage-matrix-builder3:exe:matrix-controller webserver
