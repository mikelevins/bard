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
         lib/Sort.scm \
         src/utils.scm \
         src/singleton-tree.scm \
         src/types.scm \
         src/types-primitive-types.scm \
         src/types-roles.scm \
         src/types-protocols.scm \
         src/types-alist-table.scm \
         src/type-signatures.scm \
         src/types-function.scm \
         src/types-interpreted-method.scm \
         src/types-primitive.scm \
         src/types-singleton.scm \
         src/types-generator.scm \
         src/types-structure-structs.scm \
         src/types-records.scm \
         src/types-tuples.scm \
         src/types-url.scm \
         src/value-to-struct.scm \
         src/env.scm \
         src/primitives.scm \
         src/read.scm \
         src/print.scm \
         src/special.scm \
         src/macro.scm \
         src/apply.scm \
         src/eval.scm \
         src/error.scm \
         src/protocol-addressing.scm \
         src/protocol-comparing.scm \
         src/protocol-converting.scm \
         src/protocol-creating.scm \
         src/protocol-listing.scm \
         src/protocol-mapping.scm \
         src/protocol-ordering.scm \
         src/protocol-pairing.scm \
         src/protocol-streaming.scm \
         src/protocol-text-processing.scm \
         src/protocol-typing.scm \
         src/init.scm


C_SOURCES= \
         src/version.c \
         lib/uuid.c \
         lib/Sort.c \
         src/utils.c \
         src/singleton-tree.c \
         src/types.c \
         src/types-primitive-types.c \
         src/types-roles.c \
         src/types-protocols.c \
         src/types-alist-table.c \
         src/type-signatures.c \
         src/types-function.c \
         src/types-interpreted-method.c \
         src/types-primitive.c \
         src/types-singleton.c \
         src/types-generator.c \
         src/types-structure-structs.c \
         src/types-records.c \
         src/types-tuples.c \
         src/types-url.c \
         src/value-to-struct.c \
         src/env.c \
         src/primitives.c \
         src/read.c \
         src/print.c \
         src/special.c \
         src/macro.c \
         src/apply.c \
         src/eval.c \
         src/error.c \
         src/protocol-addressing.c \
         src/protocol-comparing.c \
         src/protocol-converting.c \
         src/protocol-creating.c \
         src/protocol-listing.c \
         src/protocol-mapping.c \
         src/protocol-ordering.c \
         src/protocol-pairing.c \
         src/protocol-streaming.c \
         src/protocol-text-processing.c \
         src/protocol-typing.c \
         src/init.c \



OBJECTS= \
         version.o \
         uuid.o \
         Sort.o \
         utils.o \
         singleton-tree.o \
         types.o \
         types-primitive-types.o \
         types-roles.o \
         types-protocols.o \
         types-alist-table.o \
         type-signatures.o \
         types-function.o \
         types-interpreted-method.o \
         types-primitive.o \
         types-singleton.o \
         types-generator.o \
         types-structure-structs.o \
         types-records.o \
         types-tuples.o \
         types-url.o \
         value-to-struct.o \
         env.o \
         primitives.o \
         read.o \
         print.o \
         special.o \
         macro.o \
         apply.o \
         eval.o \
         error.o \
         protocol-addressing.o \
         protocol-comparing.o \
         protocol-converting.o \
         protocol-creating.o \
         protocol-listing.o \
         protocol-mapping.o \
         protocol-ordering.o \
         protocol-pairing.o \
         protocol-streaming.o \
         protocol-text-processing.o \
         protocol-typing.o \
         init.o

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
