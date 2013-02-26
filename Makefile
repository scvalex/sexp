CABAL := $(shell cabal-dev --version > /dev/null && echo cabal-dev || echo cabal)

all: test

.PHONY: all build dist install clean doc site p ghci

build: dist/setup-config
	rm -rf _site _cache
	$(CABAL) build

dist: test
	$(CABAL) sdist

install: build
	cabal install --force-reinstalls

clean:
	$(CABAL) clean

dist/setup-config: sexp.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	$(CABAL) configure --enable-tests || $(CABAL) install --enable-tests

doc: build
	$(CABAL) haddock

test: build
	$(CABAL) test

p:
	permamake.sh $(shell find src/ -name '*.hs') \
                     $(shell find test/ -name '*.hs') \
                     sexp-tool.hs
	             *.cabal \
                     Makefile

ghci: build
	cabal-dev ghci
