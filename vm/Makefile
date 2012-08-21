CC=/usr/local/gcc/bin/gcc
INCLUDE=./include
LIB=./lib

CFLAGS=-O4 -fomit-frame-pointer 

all:
	${CC} -I${INCLUDE} -L${LIB}  src/vm.c src/main.c -o bardvm

