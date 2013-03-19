
OPT=-O3
DEBUG=-DDEBUG -g -DTRACE
CFLAGS=${OPT}

all: cem

cem: Main.hs LC.hs VM.hs vm/cem.o
	ghc -O2 vm/trace.o vm/cem.o --make Main.hs -o cem

vm/cem.o: vm/cem.c vm/trace.c
	gcc -c vm/trace.c -o vm/trace.o ${CFLAGS}
	gcc -c vm/cem.c -o vm/cem.o ${CFLAGS}

clean: 
	find -type f \( \
		-iname "*.o" -or \
		-iname "*.hi" \) -delete
	rm cem

