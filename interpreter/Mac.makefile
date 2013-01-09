# ----------------------------------------
# Gambit
# ----------------------------------------
# we use the same host GSC to compile
# Scheme sources for all targets

GSC=/usr/local/Gambit-C/bin/gsc

# ----------------------------------------
# Mac
# ----------------------------------------

INSTALL_PATH=/Users/mikel/bin

MAC_EXECUTABLE=bard
MAC_LIBRARY=libBard.a
MAC_BUILD_DIR=builds/mac
MAC_GAMBIT_HOME=/usr/local/Gambit-C
MAC_ARCH=x86_64
MAC_TOOLS_ROOT=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain
MAC_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.7.sdk
MAC_SYSLIBROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.7.sdk

MAC_CC=${MAC_TOOLS_ROOT}/usr/bin/clang
MAC_LIBTOOL=${MAC_TOOLS_ROOT}/usr/bin/libtool

MAC_CFLAGS_LIB=-arch ${MAC_ARCH} -x objective-c -isysroot ${MAC_SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wuninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wshorten-64-to-32 -Wpointer-sign -Wno-newline-eof -fasm-blocks -mmacosx-version-min=10.6 -g -Wno-conversion -Wno-sign-conversion -I${MAC_GAMBIT_HOME}/include -D___LIBRARY

MAC_LDFLAGS_LIB=-static -arch_only ${MAC_ARCH} -syslibroot ${MAC_SYSLIBROOT} -framework Cocoa -o ${MAC_BUILD_DIR}/${MAC_LIBRARY}

MAC_CFLAGS_MAIN=-arch ${MAC_ARCH} -x objective-c -isysroot ${MAC_SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wuninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wshorten-64-to-32 -Wpointer-sign -Wno-newline-eof -fasm-blocks -mmacosx-version-min=10.6 -g -Wno-conversion -Wno-sign-conversion -I${MAC_GAMBIT_HOME}/include

MAC_LDFLAGS_MAIN=-arch ${MAC_ARCH} -isysroot ${MAC_SYSROOT} -mmacosx-version-min=10.7 -framework Cocoa -o ${MAC_BUILD_DIR}/${MAC_EXECUTABLE} -L${MAC_GAMBIT_HOME}/lib -lgambc

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

all: mac_main

install: 
	cp ${MAC_BUILD_DIR}/$(MAC_EXECUTABLE) ${INSTALL_PATH}/bard

clean:
	rm -f ${C_SOURCES}
	rm -f ${MAIN_C_SOURCES}
	rm -f ${LIB_C_SOURCES}

# -------------------
# Bard Library

mac_lib: 
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${MAC_CC} ${MAC_CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${MAC_LIBTOOL} ${MAC_LDFLAGS_LIB} ${OBJECTS} ${LIB_OBJECTS}

# -------------------
# Bard Executable

mac_main: 
	${GSC} -link ${SCHEME_SOURCES} ${MAIN_SCHEME_SOURCES}
	${MAC_CC} ${MAC_CFLAGS_MAIN} ${MAC_LDFLAGS_MAIN} ${C_SOURCES} ${MAIN_C_SOURCES}



