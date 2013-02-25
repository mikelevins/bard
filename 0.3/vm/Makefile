# ----------------------------------------
# Gambit
# ----------------------------------------

GSC=/usr/local/Gambit-C/bin/gsc
GAMBIT_HOME=/usr/local/Gambit-C

# ----------------------------------------
# compiler flags
# ----------------------------------------

CC_OPTS="-D___SINGLE_HOST -O1"

# ----------------------------------------
# sources
# ----------------------------------------

SCHEME_SOURCES=src/version.scm \
               src/banner.scm \
               lib/uuid.scm \
	       src/util.scm \
               src/stack.scm \
               src/env.scm \
               src/instructions.scm \
               src/program.scm \
               src/bardo.scm \
               src/bardvm.scm

# ----------------------------------------
# Mac build
# ----------------------------------------

CC=/usr/gcc-4.7.1/bin/gcc
LIBTOOL=/usr/gcc-4.7.1/bin/gcc-ar


all: main

clean: tidy
	rm -f bardvm

tidy:
	rm -f *.o
	rm -f *.c
	rm -f *.gvm
	rm -f *.o
	rm -f src/*.c
	rm -f src/*.o
	rm -f src/*.gvm
	rm -f lib/*.o
	rm -f lib/*.c
	rm -f lib/*.gvm

gvm:
	${GSC} -gvm -cc-options ${CC_OPTS} -exe -o bardvm ${SCHEME_SOURCES}

asm:
	${GSC} -c -S -cc-options ${CC_OPTS} -exe -o bardvm ${SCHEME_SOURCES}

obj:
	${GSC} -obj -cc-options ${CC_OPTS} ${SCHEME_SOURCES}

main: 
	${GSC} -cc-options ${CC_OPTS} -exe -o bardvm ${SCHEME_SOURCES}



