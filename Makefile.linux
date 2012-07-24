
GSC=/usr/local/Gambit-C/bin/gsc
CC=/usr/bin/gcc

CFLAGS_MAIN= 
LDFLAGS_MAIN= -L/usr/local/Gambit-C/lib -lgambc -o builds/bard

# ----------------------------------------
# Bard Common files
# ----------------------------------------

SCHEME_SOURCES= \
         src/version.scm \
         lib/uuid.scm \
         src/util/general.scm \
         src/values/types.scm \
         src/values/values.scm \
         src/eval/env.scm \
         src/values/functions.scm \
         src/repl/prims.scm \
         src/read.scm \
         src/print.scm \
         src/eval/special.scm \
         src/eval/macro.scm \
         src/eval/apply.scm \
         src/eval/eval.scm \
         src/repl/error.scm \
         src/repl/toplevel.scm \
         src/values/protocols.scm

C_SOURCES= \
         src/version.c \
         lib/uuid.c \
         src/util/general.c \
         src/values/types.c \
         src/values/values.c \
         src/eval/env.c   \
         src/values/functions.c \
         src/repl/prims.c \
         src/read.c \
         src/print.c \
         src/eval/special.c \
         src/eval/macro.c \
         src/eval/apply.c \
         src/eval/eval.c \
         src/repl/error.c \
         src/repl/toplevel.c \
         src/values/protocols.c

OBJECTS= \
         version.o \
         uuid.o \
         general.o \
         types.o \
         values.o  \
         env.o   \
         functions.o \
         prims.o \
         read.o \
         print.o \
         special.o \
         macro.o \
         apply.o \
         eval.o \
         error.o \
         toplevel.o \
         protocols.o

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

# -------------------
# Housekeeping

BUILD_DIR=builds
EXECUTABLE=bard

all: main

clean:
	rm -f ${BUILD_DIR}/${EXECUTABLE}
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

tidy:
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${LIB_C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

# -------------------
# Bard Executable

main: tidy
	${GSC} -exe ${SCHEME_SOURCES} ${MAIN_SCHEME_SOURCES}
