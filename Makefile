.PHONY: deps world bench

deps:
	@yarn

world: deps
	@bsb -make-world -w

bench:
	@node ./lib/js/bench/Bench.bs.js
