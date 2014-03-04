all: cem

cem: *.hs
	ghc -O2 -rtsopts -XLambdaCase -XTupleSections Main.hs -o cem

clean: 
	find -type f \( \
		-iname "*.o" -or \
		-iname "*.hi" -or \
		-iname "cem"  \) -delete
