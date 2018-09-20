NODE_BIN=./node_modules/.bin
BSB=$(NODE_BIN)/bsb

.PHONY: deps
deps:
	@yarn

.PHONY: docs
docs: build
	@./scripts/mk-docs.sh

.PHONY: build
build: deps
	@$(BSB) -make-world

.PHONY: world
world: deps
	@$(BSB) -make-world -w

.PHONY: test
test:
	@node lib/js/test/Test.bs.js

.PHONY: clean
clean:
	@$(BSB) -clean-world

.PHONY: cleanall
cleanall: clean
	@rm -rf node_modules lib
