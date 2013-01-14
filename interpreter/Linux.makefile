# ----------------------------------------
# Gambit
# ----------------------------------------

GSC=/usr/local/Gambit-C/bin/gsc

# ----------------------------------------
# Linux
# ----------------------------------------

CC=/usr/bin/gcc

CFLAGS_MAIN= 
LDFLAGS_MAIN= -L/usr/local/Gambit-C/lib -lgambc -o builds/bard

# ----------------------------------------
# Bard Common files
# ----------------------------------------

SCHEME_SOURCES= \
         src/version.scm \
         lib/uuid.scm \
         src/utils.scm \
         src/singleton-tree.scm \
         src/types.scm \
         src/env.scm \
         src/primitives.scm \
         src/read.scm \
         src/print.scm \
         src/special.scm \
         src/macro.scm \
         src/apply.scm \
         src/eval.scm \
         src/error.scm \
         src/protocol-comparing.scm \
         src/protocol-converting.scm \
         src/protocol-creating.scm \
         src/protocol-listing.scm \
         src/protocol-ordering.scm \
         src/protocol-pairing.scm \
         src/protocol-typing.scm \
         src/init.scm \

C_SOURCES= \
         src/version.c \
         lib/uuid.c \
         src/utils.c \
         src/singleton-tree.c \
         src/types.c \
         src/env.c \
         src/primitives.c \
         src/read.c \
         src/print.c \
         src/special.c \
         src/macro.c \
         src/apply.c \
         src/eval.c \
         src/error.c \
         src/protocol-comparing.c \
         src/protocol-converting.c \
         src/protocol-creating.c \
         src/protocol-listing.c \
         src/protocol-ordering.c \
         src/protocol-pairing.c \
         src/protocol-typing.c \
         src/init.c \

OBJECTS= \
         version.o \
         uuid.o \
         utils.o \
         singleton-tree.o \
         types.o \
         env.o \
         primitives.o \
         read.o \
         print.o \
         special.o \
         macro.o \
         apply.o \
         eval.o \
         error.o \
         protocol-comparing.o \
         protocol-converting.o \
         protocol-creating.o \
         protocol-listing.o \
         protocol-ordering.o \
         protocol-pairing.o \
         protocol-typing.o \
         init.o \

# ----------------------------------------
# Inputs to the Bard executable
# ----------------------------------------

MAIN_SCHEME_SOURCES= \
         src/bard.scm \
         src/main.scm

MAIN_C_SOURCES= \
         src/bard.c \
         src/main.c \
         src/main_.c

# ----------------------------------------
# make rules
# ----------------------------------------

BUILD_DIR=builds/linux
EXECUTABLE=bard
INSTALL_PATH=~/bin

all: main

install: 
	cp ${BUILD_DIR}/bard ${INSTALL_PATH}/bard

clean:
	rm -f ${C_SOURCES}
	rm -f ${MAIN_C_SOURCES}
	rm -f ${LIB_C_SOURCES}

# -------------------
# Bard Executable

main: 
	${GSC} -exe ${SCHEME_SOURCES} ${MAIN_SCHEME_SOURCES}
	mv src/main ${BUILD_DIR}/bard
