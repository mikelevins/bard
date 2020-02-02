# ----------------------------------------
# Gambit
# ----------------------------------------

GSC=/usr/local/Gambit/bin/gsc
GAMBIT_HOME=/usr/local/Gambit

# ----------------------------------------
# Mac
# ----------------------------------------

BUILD_DIR=./build
EXECUTABLE=bard

ARCH=x86_64
TOOLS_ROOT=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain
SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk
SYSLIBROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk

CC=${TOOLS_ROOT}/usr/bin/clang

LIBTOOL=${TOOLS_ROOT}/usr/bin/libtool

CFLAGS=-arch ${ARCH} -x objective-c -isysroot ${SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wuninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wshorten-64-to-32 -Wpointer-sign -Wno-newline-eof -fasm-blocks -mmacosx-version-min=10.11 -g -Wno-conversion -Wno-sign-conversion -I${GAMBIT_HOME}/include
LDFLAGS_MAIN=-arch ${ARCH} -isysroot ${SYSROOT} -mmacosx-version-min=10.11 -framework Cocoa -o ${BUILD_DIR}/${EXECUTABLE} -L${GAMBIT_HOME}/lib -lgambit

# ----------------------------------------
# Bard Common files
# ----------------------------------------

SCHEME_SOURCES= \
         src/version.scm \
         src/opcodes.scm \
         src/global.scm \
         src/env.scm \
         src/instr.scm \
         src/return.scm \
         src/fn.scm \
         src/bardvm.scm \
         src/testvm.scm


C_SOURCES= \
         src/version.c \
         src/opcodes.c \
         src/global.c \
         src/env.c \
         src/instr.c \
         src/return.c \
         src/fn.c \
         src/bardvm.c \
         src/testvm.c



OBJECTS= \
         src/version.o \
         src/opcodes.o \
         src/global.o \
         src/env.o \
         src/instr.o \
         src/return.o \
         src/fn.o \
         src/bardvm.o \
         src/testvm.o


# ----------------------------------------
# make rules
# ----------------------------------------

BUILD_DIR=.
EXECUTABLE=testvm

all: main

clean:
	rm -f ${C_SOURCES}
	rm -f ${OBJECTS}

# -------------------
# Bard Executable


main: 
	${GSC} -exe ${SCHEME_SOURCES} 



