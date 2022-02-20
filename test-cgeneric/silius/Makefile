CC = gcc
CFLAGS = -Wall -fpic -g3 -O3 -mtune=native -fopenmp -funroll-all-loops

alpha.so : alpha.o
	$(CC) -shared -o $@ $^
