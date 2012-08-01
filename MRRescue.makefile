# ----------------------------------------
# Gambit
# ----------------------------------------
# we use the same host GSC to compile
# Scheme sources for all targets

GSC=/usr/local/gambit/macosx/bin/gsc

# ----------------------------------------
# MRRescue
# ----------------------------------------
# for targets that build components for
# the MRRescue project, we need the path
# to the project

MRRESCUE_PATH=/Users/mikel/Projects/nelson/nelson/project/MRRescue

# ----------------------------------------
# iOS
# ----------------------------------------

# Device

IOS_BUILD_DIR=builds/ios
IOS_LIBRARY=libBard.a

IOS_DEVICE_LIBRARY=libBard.a
IOS_DEVICE_BUILD_DIR=builds/ios/device
IOS_DEVICE_GAMBIT_HOME=/usr/local/gambit/ios
IOS_DEVICE_ARCH=armv7
IOS_DEVICE_TOOLS_ROOT=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain
IOS_DEVICE_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk
IOS_DEVICE_SYSLIBROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk

IOS_DEVICE_CC=${IOS_DEVICE_TOOLS_ROOT}/usr/bin/clang
IOS_DEVICE_LIBTOOL=${IOS_DEVICE_TOOLS_ROOT}/usr/bin/libtool

IOS_DEVICE_CFLAGS_LIB=-arch ${IOS_DEVICE_ARCH} -x objective-c  -isysroot ${IOS_DEVICE_SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wno-uninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wno-shorten-64-to-32 -Wpointer-sign -Wno-newline-eof -fasm-blocks -g -Wno-conversion -Wno-sign-conversion -I${IOS_DEVICE_GAMBIT_HOME}/include -D___LIBRARY

IOS_DEVICE_LDFLAGS_LIB=-static -arch_only ${IOS_DEVICE_ARCH} -syslibroot ${IOS_DEVICE_SYSLIBROOT} -ObjC -all_load -framework UIKit -o ${IOS_DEVICE_BUILD_DIR}/${IOS_DEVICE_LIBRARY}

# Simulator

IOS_SIM_LIBRARY=libBard.a
IOS_SIM_BUILD_DIR=builds/ios/simulator
IOS_SIM_GAMBIT_HOME=/usr/local/gambit/ios
IOS_SIM_ARCH=i386
IOS_SIM_TOOLS_ROOT=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain
IOS_SIM_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.1.sdk
IOS_SIM_SYSLIBROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.1.sdk

IOS_SIM_CC=${IOS_SIM_TOOLS_ROOT}/usr/bin/clang
IOS_SIM_LIBTOOL=${IOS_SIM_TOOLS_ROOT}/usr/bin/libtool

IOS_SIM_CFLAGS_LIB=-arch ${IOS_SIM_ARCH} -x objective-c  -isysroot ${IOS_SIM_SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wno-uninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wno-shorten-64-to-32 -Wpointer-sign -Wno-newline-eof -g -Wno-conversion -Wno-sign-conversion -I${IOS_SIM_GAMBIT_HOME}/include -D___LIBRARY

IOS_SIM_LDFLAGS_LIB=-static -arch_only ${IOS_SIM_ARCH} -syslibroot ${IOS_SIM_SYSLIBROOT} -ObjC -all_load -framework Foundation -L${IOS_SIM_GAMBIT_HOME}/lib -lgambc -o ${IOS_SIM_BUILD_DIR}/${IOS_SIM_LIBRARY}

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
         cbard/c.c \
         src/bard.c \
         src/bard_.c 

LIB_OBJECTS= \
         cbard_errors.o \
         cbard.o \
         c.o \
         bard.o \
         bard_.o 

# ----------------------------------------
# make rules
# ----------------------------------------

all: mrrescue_lib
	make tidy

# -------------------
# Housekeeping

clean:
	rm -f ${C_SOURCES}
	rm -f ${LIB_C_SOURCES}
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}
	rm -f ${IOS_LIBRARY}
	rm -f ${IOS_DEVICE_LIBRARY}
	rm -f ${IOS_SIM_LIBRARY}

tidy:
	rm -f ${C_SOURCES}
	rm -f ${LIB_C_SOURCES}
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}

# -------------------
# Mrrescue components

all: mrrescue_lib

install: 
	cp ${IOS_BUILD_DIR}/$(IOS_LIBRARY) ${MRRESCUE_PATH}/bard/lib/libBard.a
	cp include/cbard.h ${MRRESCUE_PATH}/bard/include/cbard.h

mrrescue_lib: mrrescue_device_lib mrrescue_sim_lib
	lipo -create ${IOS_DEVICE_BUILD_DIR}/$(IOS_DEVICE_LIBRARY) ${IOS_SIM_BUILD_DIR}/$(IOS_SIM_LIBRARY) -output ${IOS_BUILD_DIR}/$(IOS_LIBRARY)

# Mrrescue Device

mrrescue_device_lib: 
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${IOS_DEVICE_CC} ${IOS_DEVICE_CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${IOS_DEVICE_LIBTOOL} ${IOS_DEVICE_LDFLAGS_LIB} ${OBJECTS} ${LIB_OBJECTS}

# Mrrescue Simulator

mrrescue_sim_lib: 
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES} 
	${IOS_SIM_CC} ${IOS_SIM_CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES} 
	${IOS_SIM_LIBTOOL} ${IOS_SIM_LDFLAGS_LIB} ${OBJECTS} ${LIB_OBJECTS}

