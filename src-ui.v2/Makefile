# Assumes that tools compatible w/
#
# - bower 1.8.4
# - pulp 12.3.0
# - purescript 0.11.7
# - purescript-psa 0.7.3 (optional)
#
# are installed

.PHONY: all
all:
	bower install
	pulp browserify -O --to ../ui.v2/ui.js

.PHONY: clean
clean:
	rm -rf output .pulp-cache .psci* .psa-stash

.PHONY: distclean
distclean: clean
	rm -rf bower_components
