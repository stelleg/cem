all: cem

cem: *.hs
	cabal configure 
	cabal build

install: 
	cabal install

clean: 
	rm -rf dist
	find -type f \( \
		-iname "*.o" -or \
		-iname "*.hi" -or \
		-iname "cem"  \) -delete
