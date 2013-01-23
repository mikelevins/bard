# ----------------------------------------
# Gambit
# ----------------------------------------

GSC=/usr/local/Gambit-C/bin/gsc
GAMBIT_HOME=/usr/local/Gambit-C

# ----------------------------------------
# Mac
# ----------------------------------------

CC=/usr/gcc-4.7.1/bin/gcc
LIBTOOL=/usr/gcc-4.7.1/bin/gcc-ar


all: main

clean:
	rm -f *.o
	rm -f src/*.o
	rm -f src/*.c
	rm -f src/*.gvm

obj:
	${GSC} -obj src/bardvm.scm src/main.scm

main: 
	${GSC} -cc-options "-D___SINGLE_HOST -O1" -exe -o bardvm src/instructions.scm src/bardvm.scm src/main.scm



