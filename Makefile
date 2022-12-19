build:
	cabal build
run: build
	cabal run scheme 1
test: build
	cabal test
