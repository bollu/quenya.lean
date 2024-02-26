.PHONY: eg1.o

run: eg1.out
	./eg1.out

build:
	lake build

eg1.o: build
	lake exe elfbuilder

eg1.out: eg1.o
	ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o eg1.out eg1.o -lc
