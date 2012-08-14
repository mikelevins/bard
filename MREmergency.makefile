# ----------------------------------------
# Gambit
# ----------------------------------------

GSC=/usr/local/gambit/macosx/bin/gsc

# ----------------------------------------
# MREmergency
# ----------------------------------------
# for targets that build components for
# the MREmergency project, we need the path
# to the project

INSTALL_PATH=/Users/mikel/Projects/nelson/MREmergency

# ----------------------------------------
# Common
# ----------------------------------------

BUILD_DIR=builds/ios
LIBRARY=libBard.a

# ----------------------------------------
# iOS Device
# ----------------------------------------

DEV_LIBRARY=libBard_dev.a
DEV_BUILD_DIR=builds/ios/device
DEV_GAMBIT_HOME=/usr/local/gambit/ios
DEV_ARCH=armv7
DEV_TOOLS_ROOT=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain
DEV_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk
DEV_SYSLIBROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk

DEV_CC=${DEV_TOOLS_ROOT}/usr/bin/clang
DEV_LIBTOOL=${DEV_TOOLS_ROOT}/usr/bin/libtool

DEV_CFLAGS_WARN=-Wno-trigraphs -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wno-uninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wno-shorten-64-to-32 -Wpointer-sign -Wno-newline-eof -Wno-conversion -Wno-sign-conversion

DEV_CFLAGS_PLATFORM=-arch ${DEV_ARCH} -x objective-c  -isysroot ${DEV_SYSROOT} -fmessage-length=0 -std=gnu99 -fpascal-strings -O0 -fasm-blocks -g

DEV_CFLAGS=${DEV_CFLAGS_PLATFORM} ${DEV_CFLAGS_WARN} -I${DEV_GAMBIT_HOME}/include -D___LIBRARY

DEV_LDFLAGS_PLATFORM=-static -arch_only ${DEV_ARCH} -syslibroot ${DEV_SYSLIBROOT} -ObjC -all_load -framework UIKit

DEV_LDFLAGS=${DEV_LDFLAGS_PLATFORM} -o ${DEV_BUILD_DIR}/${DEV_LIBRARY}

# ----------------------------------------
# iOS Simulator
# ----------------------------------------

SIM_LIBRARY=libBard_sim.a
SIM_BUILD_DIR=builds/ios/simulator
SIM_GAMBIT_HOME=/usr/local/gambit/ios
SIM_ARCH=i386
SIM_TOOLS_ROOT=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain
SIM_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.1.sdk
SIM_SYSLIBROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.1.sdk

SIM_CC=${SIM_TOOLS_ROOT}/usr/bin/clang
SIM_LIBTOOL=${SIM_TOOLS_ROOT}/usr/bin/libtool

SIM_CFLAGS_WARN=-Wno-trigraphs -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wno-uninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wno-shorten-64-to-32 -Wpointer-sign -Wno-newline-eof -Wno-conversion -Wno-sign-conversion

SIM_CFLAGS_PLATFORM=-arch ${SIM_ARCH} -x objective-c  -isysroot ${SIM_SYSROOT} -fmessage-length=0 -std=gnu99  -fpascal-strings 

SIM_CFLAGS=${SIM_CFLAGS_WARN} ${SIM_CFLAGS_PLATFORM} -O0 -g -I${SIM_GAMBIT_HOME}/include -D___LIBRARY

SIM_LDFLAGS_PLATFORM=-static -arch_only ${SIM_ARCH} -syslibroot ${SIM_SYSLIBROOT} -ObjC -all_load -framework Foundation

SIM_LDFLAGS=${SIM_LDFLAGS_PLATFORM} -L${SIM_GAMBIT_HOME}/lib -lgambc -o ${SIM_BUILD_DIR}/${SIM_LIBRARY}

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
# Inputs to the Bard library
# ----------------------------------------

LIB_SCHEME_SOURCES= \
         cbard/cbard_errors.scm \
         cbard/cbard.scm \
         cbard/c.scm \
         src/bard.scm 

LIB_C_SOURCES= \
         cbard/cbard_errors.c \
         cbard/cbard.c \
         cbard/csupport.c \
         cbard/c.c \
         src/bard.c \
         src/bard_.c 

LIB_OBJECTS= \
         cbard_errors.o \
         cbard.o \
         csupport.o \
         c.o \
         bard.o \
         bard_.o 

# ----------------------------------------
# make rules
# ----------------------------------------

all: lib


install: 
	cp ${BUILD_DIR}/$(LIBRARY) ${INSTALL_PATH}/bard/lib/libBard.a
	cp include/cbard.h ${INSTALL_PATH}/bard/include/cbard.h

# -------------------
# components

lib: device_lib sim_lib
	lipo -create ${DEV_BUILD_DIR}/$(DEV_LIBRARY) ${SIM_BUILD_DIR}/$(SIM_LIBRARY) -output ${BUILD_DIR}/$(LIBRARY)

# Device

device_lib: 
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${DEV_CC} ${DEV_CFLAGS} -I./include -c ${C_SOURCES} ${LIB_C_SOURCES}
	${DEV_LIBTOOL} ${DEV_LDFLAGS} ${OBJECTS} ${LIB_OBJECTS}

# Simulator

sim_lib: 
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES} 
	${SIM_CC} ${SIM_CFLAGS} -I./include -c ${C_SOURCES} ${LIB_C_SOURCES} 
	${SIM_LIBTOOL} ${SIM_LDFLAGS} ${OBJECTS} ${LIB_OBJECTS}

# ----------------------------------------
# housekeeping
# ----------------------------------------

clean:
	rm -f *.o
	rm -f cbard/c.c cbard/cbard.c cbard/cbard_errors.c cbard/cbardtest 
	rm -f src/*.c 
	rm -f lib/*.c 
	rm -f src/eval/*.c src/repl/*.c src/util/*.c src/values/*.c 
