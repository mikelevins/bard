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
         src/types-primitive-types.scm \
         src/types-classes.scm \
         src/types-protocols.scm \
         src/types-alist-table.scm \
         src/type-signatures.scm \
         src/types-function.scm \
         src/types-interpreted-method.scm \
         src/types-primitive.scm \
         src/types-singleton.scm \
         src/types-generator.scm \
         src/types-structure-schemas.scm \
         src/types-records.scm \
         src/types-tuples.scm \
         src/value-to-schema.scm \
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
         src/protocol-mapping.scm \
         src/protocol-ordering.scm \
         src/protocol-pairing.scm \
         src/protocol-typing.scm \
         src/init.scm

C_SOURCES= \
         src/version.c \
         lib/uuid.c \
         src/utils.c \
         src/singleton-tree.c \
         src/types.c \
         src/types-primitive-types.c \
         src/types-classes.c \
         src/types-protocols.c \
         src/types-alist-table.c \
         src/type-signatures.c \
         src/types-function.c \
         src/types-interpreted-method.c \
         src/types-primitive.c \
         src/types-singleton.c \
         src/types-generator.c \
         src/types-structure-schemas.c \
         src/types-records.c \
         src/types-tuples.c \
         src/value-to-schema.c \
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
         src/protocol-mapping.c \
         src/protocol-ordering.c \
         src/protocol-pairing.c \
         src/protocol-typing.c \
         src/init.c

OBJECTS= \
         src/version.o \
         lib/uuid.o \
         src/utils.o \
         src/singleton-tree.o \
         src/types.o \
         src/types-primitive-types.o \
         src/types-classes.o \
         src/types-protocols.o \
         src/types-alist-table.o \
         src/type-signatures.o \
         src/types-function.o \
         src/types-interpreted-method.o \
         src/types-primitive.o \
         src/types-singleton.o \
         src/types-generator.o \
         src/types-structure-schemas.o \
         src/types-records.o \
         src/types-tuples.o \
         src/value-to-schema.o \
         src/env.o \
         src/primitives.o \
         src/read.o \
         src/print.o \
         src/special.o \
         src/macro.o \
         src/apply.o \
         src/eval.o \
         src/error.o \
         src/protocol-comparing.o \
         src/protocol-converting.o \
         src/protocol-creating.o \
         src/protocol-listing.o \
         src/protocol-mapping.o \
         src/protocol-ordering.o \
         src/protocol-pairing.o \
         src/protocol-typing.o \
         src/init.o

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
