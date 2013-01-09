# ----------------------------------------
# Gambit
# ----------------------------------------

GSC=/usr/local/Gambit-C/bin/gsc
GAMBIT_HOME=/usr/local/Gambit-C

# ----------------------------------------
# Mac
# ----------------------------------------

ARCH=x86_64
TOOLS_ROOT=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain
SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.7.sdk
SYSLIBROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.7.sdk

CC=${TOOLS_ROOT}/usr/bin/clang

LIBTOOL=${TOOLS_ROOT}/usr/bin/libtool

CFLAGS_LIB=-arch ${ARCH} -x objective-c -isysroot ${SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wuninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wshorten-64-to-32 -Wpointer-sign -Wno-newline-eof -fasm-blocks -mmacosx-version-min=10.6 -g -Wno-conversion -Wno-sign-conversion -I${GAMBIT_HOME}/include -D___LIBRARY
LDFLAGS_LIB=-static -arch_only ${ARCH} -syslibroot ${SYSLIBROOT} -framework Cocoa -o ${BUILD_DIR}/${LIBRARY}

CFLAGS_MAIN=-arch ${ARCH} -x objective-c -isysroot ${SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wuninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wshorten-64-to-32 -Wpointer-sign -Wno-newline-eof -fasm-blocks -mmacosx-version-min=10.6 -g -Wno-conversion -Wno-sign-conversion -I${GAMBIT_HOME}/include
LDFLAGS_MAIN=-arch ${ARCH} -isysroot ${SYSROOT} -mmacosx-version-min=10.7 -framework Cocoa -o ${BUILD_DIR}/${EXECUTABLE} -L${GAMBIT_HOME}/lib -lgambc

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
         protocol-listing.o \
         protocol-ordering.o \
         protocol-pairing.o \
         protocol-typing.o \
         init.o \



# ----------------------------------------
# Inputs to the Bard library
# ----------------------------------------

LIB_SCHEME_SOURCES= \
         c_api/bard_api.scm \
         c_api/bard_c_api.scm \
         src/bard.scm 

LIB_C_SOURCES= \
         c_api/bard_api.c \
         c_api/bard_c_api.c \
         src/bard.c \
         src/bard_.c 

LIB_OBJECTS= \
         bard_api.o \
         bard.o \
         bard_.o 

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

BUILD_DIR=builds/mac
EXECUTABLE=bard
LIBRARY=libBard.a
INSTALL_PATH=~/bin

all: main

install: 
	cp ${BUILD_DIR}/$(EXECUTABLE) ${INSTALL_PATH}/bard

clean:
	rm -f ${C_SOURCES}
	rm -f ${MAIN_C_SOURCES}
	rm -f ${LIB_C_SOURCES}

# -------------------
# Bard Library

mac_lib: 
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${CC} ${CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${LIBTOOL} ${LDFLAGS_LIB} ${OBJECTS} ${LIB_OBJECTS}

# -------------------
# Bard Executable

main: 
	${GSC} -link ${SCHEME_SOURCES} ${MAIN_SCHEME_SOURCES}
	${CC} ${CFLAGS_MAIN} ${LDFLAGS_MAIN} ${C_SOURCES} ${MAIN_C_SOURCES}



