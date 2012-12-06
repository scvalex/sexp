all: build

.PHONY: all build dist install clean doc site p

build: dist/setup-config
	rm -rf _site _cache
	cabal-dev build

dist:
	cabal-dev sdist

install: build
	cabal-dev install

clean:
	cabal-dev clean

dist/setup-config: sexp.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	cabal-dev configure || cabal-dev install

doc: build
	cabal-dev haddock

p:
	permamake.sh *.hs *.cabal Makefile
