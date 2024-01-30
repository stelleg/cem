all: cem

cabalflags=--overwrite-policy=always

cem: *.hs
	cabal configure $(cabalflags)
	cabal build 

install: 
	cabal install $(cabalflags) 

clean: 
	rm -rf dist
	find -type f \( \
		-iname "*.o" -or \
		-iname "*.hi" -or \
		-iname "cem"  \) -delete
