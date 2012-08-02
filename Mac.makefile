# ----------------------------------------
# Gambit
# ----------------------------------------
# we use the same host GSC to compile
# Scheme sources for all targets

GSC=/usr/local/gambit/macosx/bin/gsc

# ----------------------------------------
# 
# ----------------------------------------

EXECUTABLE=bard
LIBRARY=libBard.a
BUILD_DIR=builds/mac
GAMBIT_HOME=/usr/local/gambit/macosx
ARCH=x86_64

SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.7.sdk
SYSLIBROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.7.sdk

CC=/usr/local/gcc/bin/gcc

CFLAGS_WARNINGS=-Wno-trigraphs -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wuninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-sign-compare -Wpointer-sign -Wno-conversion -Wno-sign-conversion

CFLAGS_LANG=-x objective-c -std=gnu99 -fmessage-length=0 

CFLAGS_PLATFORM=-arch ${ARCH}  -isysroot ${SYSROOT} -mmacosx-version-min=10.7  -I${GAMBIT_HOME}/include

CFLAGS=${CFLAGS_LANG} ${CFLAGS_PLATFORM} -O0 ${CFLAGS_WARNINGS} -g

LDFLAGS_PLATFORM=-arch ${ARCH} -isysroot ${SYSROOT} -mmacosx-version-min=10.7 -framework Cocoa  -L${GAMBIT_HOME}/lib -lgambc

LDFLAGS=${LDFLAGS_PLATFORM} -o ${BUILD_DIR}/${EXECUTABLE}

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

MAIN_OBJECTS= \
         bard.o \
         main.o \
         main_.o

# ----------------------------------------
# make rules
# ----------------------------------------

all: main
	make tidy

# -------------------
# Housekeeping

clean:
	rm -f ${BUILD_DIR}/${EXECUTABLE}
	rm -f ${BUILD_DIR}/$(LIBRARY)
	rm -f ${OBJECTS}
	rm -f ${MAIN_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

tidy:
	rm -f ${OBJECTS}
	rm -f ${MAIN_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

# -------------------
# Bard Executable

# Mac

install: main
	cp ${BUILD_DIR}/$(EXECUTABLE) ~/bin/bard

main: tidy
	${GSC} -link ${SCHEME_SOURCES} ${MAIN_SCHEME_SOURCES}
	${CC} ${CFLAGS} ${LDFLAGS} ${C_SOURCES} ${MAIN_C_SOURCES}


