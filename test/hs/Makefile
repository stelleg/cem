TESTS= exp-true \
	     exp-false \
       fact-true

all: ${TESTS} *.hs

${TESTS}: Church.o Y.o
	ghc -O2 -c $@.hs
	ghc -O2 -rtsopts -with-rtsopts='-K1024M' $^ -o $@ $@.o

Church.o: Church.hs
	ghc -O2 -c Church.hs

Y.o: Y.hs
	ghc -O2 -c Y.hs

clean: 
	rm *.hi *.o
