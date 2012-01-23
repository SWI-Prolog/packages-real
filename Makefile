CFLAGS=-I${local}/lib64/R/include/ -L${local}/lib64 -Wall
LDFLAGS=-L${local}/lib64
# CFLAGS=-I/usr/share/R/include -Wall -gdwarf-2 -g3

real.so: C/real.c
	swipl-ld $(CFLAGS) -shared -o $@ C/real.c -lR
