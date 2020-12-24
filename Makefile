UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  GAMBIT_HOME=/usr/local/Gambit
endif
ifeq ($(UNAME_S),Linux)
  GAMBIT_HOME=/usr/local/Gambit
endif

GSC=${GAMBIT_HOME}/bin/gsc
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib
GCC = gcc

SCHEME_SOURCES= \
         src/version.scm \
         lib/uuid.scm \
         lib/Sort.scm \
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
         src/types-structure-structures.scm \
         src/types-url.scm \
         src/value-to-structure.scm \
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
         src/init.scm \
         src/bard.scm 


C_SOURCES= \
         src/version.c \
         lib/uuid.c \
         lib/Sort.c \
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
         src/types-structure-structures.c \
         src/types-url.c \
         src/value-to-structure.c \
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
         src/bard.c 



OBJECTS= \
         version.o \
         uuid.o \
         Sort.o \
         utils.o \
         singleton-tree.o \
         types.o \
         types-primitive-types.o \
         types-classes.o \
         types-protocols.o \
         types-alist-table.o \
         type-signatures.o \
         types-function.o \
         types-interpreted-method.o \
         types-primitive.o \
         types-singleton.o \
         types-generator.o \
         types-structure-structures.o \
         types-url.o \
         value-to-structure.o \
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
         init.o \
         src/bard.o

exe:
	${GSC} -f -o bard -exe ${SCHEME_SOURCES} src/main.scm


clean:
	rm -f bard
	rm -f ${C_SOURCES}
	rm -f ${OBJS}
	rm -f src/*.o
	rm -f src/*.o1
	rm -f src/*.o2
	rm -f src/*.o
	rm -f src/*.o1
	rm -f src/*.o2
	rm -f *.o
	rm -f *.o1
	rm -f *.o2
	rm -f *~


obj: compile_scheme
	${GSC} -obj -cc-options ${CFLAGS} ${C_SOURCES}

compile_scheme:
	${GSC} -link ${SCHEME_SOURCES}
