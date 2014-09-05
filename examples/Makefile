.PHONY: all

all: $(patsubst %.lc, %, $(wildcard *.lc))

%: %.lc
	cem -lc $<

clean:
	find . -perm +100 -type f -delete
	find . -type f \( -iname "*.o" -or -iname "*.hi" \) -delete
	find . -type f \( -iname "*.grin" -or -iname "*.core" \) -delete
