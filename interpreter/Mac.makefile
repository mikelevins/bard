# ----------------------------------------
# Gambit
# ----------------------------------------
# we use the same host GSC to compile
# Scheme sources for all targets

GSC=/usr/local/gambit/macosx/bin/gsc

# ----------------------------------------
# Mac
# ----------------------------------------

INSTALL_PATH=/Users/mikel/bin

MAC_EXECUTABLE=bard
MAC_LIBRARY=libBard.a
MAC_BUILD_DIR=builds/mac
MAC_GAMBIT_HOME=/usr/local/gambit/macosx
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
         src/general.scm \
         src/types.scm \
         src/values.scm \
         src/env.scm \
         src/functions.scm \
         src/prims.scm \
         src/read.scm \
         src/print.scm \
         src/special.scm \
         src/macro.scm \
         src/apply.scm \
         src/eval.scm \
         src/error.scm \
         src/toplevel.scm \
         src/protocols.scm 



C_SOURCES= \
         src/version.c \
         lib/uuid.c \
         src/general.c \
         src/types.c \
         src/values.c \
         src/env.c   \
         src/functions.c \
         src/prims.c \
         src/read.c \
         src/print.c \
         src/special.c \
         src/macro.c \
         src/apply.c \
         src/eval.c \
         src/error.c \
         src/toplevel.c \
         src/protocols.c 



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
	rm -rf ${MAC_BUILD_DIR}/*

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



