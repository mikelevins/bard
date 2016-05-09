# ----------------------------------------
# Gambit
# ----------------------------------------

GSC=/usr/local/bin/gsc
GAMBIT_HOME=/usr/local

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

CFLAGS_LIB=-arch ${ARCH} -x objective-c -isysroot ${SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wuninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wshorten-64-to-32 -Wpointer-sign -Wno-newline-eof -fasm-blocks -mmacosx-version-min=10.11 -g -Wno-conversion -Wno-sign-conversion -I${GAMBIT_HOME}/include -D___LIBRARY
LDFLAGS_LIB=-static -arch_only ${ARCH} -syslibroot ${SYSLIBROOT} -framework Cocoa -o ${BUILD_DIR}/${LIBRARY}

CFLAGS_MAIN=-arch ${ARCH} -x objective-c -isysroot ${SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wuninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wshorten-64-to-32 -Wpointer-sign -Wno-newline-eof -fasm-blocks -mmacosx-version-min=10.11 -g -Wno-conversion -Wno-sign-conversion -I${GAMBIT_HOME}/include
LDFLAGS_MAIN=-arch ${ARCH} -isysroot ${SYSROOT} -mmacosx-version-min=10.11 -framework Cocoa -o ${BUILD_DIR}/${EXECUTABLE} -L${GAMBIT_HOME}/lib -lgambit

# ----------------------------------------
# Bard Common files
# ----------------------------------------

SCHEME_SOURCES= \
         src/version.scm

C_SOURCES= \
         src/version.c

OBJECTS= \
         version.o

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
INSTALL_PATH=~/bin

all: main

install: 
	cp ${BUILD_DIR}/$(EXECUTABLE) ${INSTALL_PATH}/bard

clean:
	rm -f ${C_SOURCES}
	rm -f ${MAIN_C_SOURCES}
	rm -f ${LIB_C_SOURCES}
	rm -rf ${BUILD_DIR}

# -------------------
# Bard Library

mac_lib: 
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${CC} ${CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${LIBTOOL} ${LDFLAGS_LIB} ${OBJECTS} ${LIB_OBJECTS}

# -------------------
# Bard Executable


main: 
	mkdir -p ${BUILD_DIR}
	${GSC} -link ${SCHEME_SOURCES} ${MAIN_SCHEME_SOURCES}
	${CC} ${CFLAGS_MAIN} ${LDFLAGS_MAIN} ${C_SOURCES} ${MAIN_C_SOURCES}



