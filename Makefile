CC=gcc

# GCC Debugging options
opt: CFLAGS += -O3
opt: cem
debug: CFLAGS += -DDEBUG -g
debug: cem
trace: CFLAGS += -DTRACE
trace: debug

# Optimization options
enteropt: CFLAGS += -DENTEROPT
enteropt: opt
collapsed: CFLAGS += -DCOLLAPSED
collapsed: opt

all: opt

cem: Main.hs LC.hs VM.hs vm/cem.o
	ghc -O2 vm/trace.o vm/cem.o --make Main.hs -o cem

vm/cem.o: vm/cem.c vm/trace.c
	${CC} -c vm/trace.c -o vm/trace.o ${CFLAGS}
	${CC} -c vm/cem.c -o vm/cem.o ${CFLAGS}

clean: 
	find -type f \( \
		-iname "*.o" -or \
		-iname "*.hi" \) -delete
	rm cem

