CC=/usr/local/gcc/bin/gcc
INCLUDE=./include
LIB=./lib

CFLAGS=-c -O4 -fomit-frame-pointer -Wno-deprecated
LDFLAGS=-o bardvm -lobjc -Wno-deprecated

all:
	${CC} ${CFLAGS} -I${INCLUDE} -L${LIB}  src/vm.c src/main.m
	${CC} ${LDFLAGS} -L${LIB}  vm.o main.o


