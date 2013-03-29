TESTS= exp-true \
			 exp-false \
			 fact-true \
			 tak-false \
			 tarai-true


profile: GHCFLAGS += -prof -fprof-auto
profile: opt

opt: GHCFLAGS += -O2 -rtsopts -with-rtsopts='-k2G' -with-rtsopts='-H8G'
opt: ${TESTS} Church.o

all: opt

${TESTS}: *.hs Church.o
	ghc -c $@.hs $(GHCFLAGS)
	ghc $(GHCFLAGS) Church.o -o $@ $@.o

Church.o: Church.hs
	ghc -c $(GHCFLAGS) Church.hs

clean: 
	rm *.hi *.o
