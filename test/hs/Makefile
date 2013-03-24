TESTS= exp-true \
			 exp-false \
			 fact-true \
			 tak-true

GHCFLAGS=-rtsopts -with-rtsopts='-K2G' '-H8G'

profile: GHCFLAGS += -prof -fprof-auto
profile: ${TESTS} Church.o

opt: GHCFLAGS += -O2
opt: ${TESTS} Church.o

all: ${TESTS} *.hs

${TESTS}: *.hs Church.o
	ghc -c $@.hs $(GHCFLAGS)
	ghc $(GHCFLAGS) Church.o -o $@ $@.o

Church.o: Church.hs
	ghc -c $(GHCFLAGS) Church.hs

clean: 
	rm *.hi *.o
