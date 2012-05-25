# ----------------------------------------
# Gambit
# ----------------------------------------
# we use the same host GSC to compile
# Scheme sources for all targets

GSC=/usr/local/gambit/macosx/bin/gsc

# ----------------------------------------
# Mac
# ----------------------------------------

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
# iOS
# ----------------------------------------

# Common

IOS_LIBRARY=libBard.a
IOS_BUILD_DIR=builds/ios

# Device

IOS_DEVICE_LIBRARY=libBard_ios.a
IOS_DEVICE_BUILD_DIR=builds/ios/device
IOS_DEVICE_GAMBIT_HOME=/usr/local/gambit/ios
IOS_DEVICE_ARCH=armv7
IOS_DEVICE_TOOLS_ROOT=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain
IOS_DEVICE_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk
IOS_DEVICE_SYSLIBROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk

IOS_DEVICE_CC=${IOS_DEVICE_TOOLS_ROOT}/usr/bin/clang
IOS_DEVICE_LIBTOOL=${IOS_DEVICE_TOOLS_ROOT}/usr/bin/libtool

IOS_DEVICE_CFLAGS_LIB=-arch ${IOS_DEVICE_ARCH} -x objective-c  -isysroot ${IOS_DEVICE_SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wno-uninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wno-shorten-64-to-32 -Wpointer-sign -Wno-newline-eof -g -Wno-conversion -Wno-sign-conversion -I${IOS_DEVICE_GAMBIT_HOME}/include -D___LIBRARY

IOS_DEVICE_LDFLAGS_LIB=-static -arch_only ${IOS_DEVICE_ARCH} -syslibroot ${IOS_DEVICE_SYSLIBROOT=} -ObjC -framework Foundation -L${IOS_DEVICE_GAMBIT_HOME}/lib -lgambc -o ${IOS_DEVICE_BUILD_DIR}/${IOS_DEVICE_LIBRARY}

# Simulator

IOS_SIM_LIBRARY=libBard_ios_sim.a
IOS_SIM_BUILD_DIR=builds/ios/simulator
IOS_SIM_GAMBIT_HOME=/usr/local/gambit/iosSimulator
IOS_SIM_ARCH=i386
IOS_SIM_TOOLS_ROOT=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain
IOS_SIM_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.1.sdk

IOS_SIM_CC=${IOS_SIM_TOOLS_ROOT}/usr/bin/clang
IOS_SIM_LIBTOOL=${IOS_SIM_TOOLS_ROOT}/usr/bin/libtool

IOS_SIM_CFLAGS_LIB=-arch ${IOS_SIM_ARCH} -x objective-c  -isysroot ${IOS_SIM_SYSROOT} -fmessage-length=0 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -Wno-missing-field-initializers -Wno-missing-prototypes -Wreturn-type -Wformat -Wno-missing-braces -Wparentheses -Wswitch -Wno-uninitialized -Wno-unknown-pragmas -Wno-shadow -Wno-four-char-constants -Wno-sign-compare -Wno-shorten-64-to-32 -Wpointer-sign -Wno-newline-eof -g -Wno-conversion -Wno-sign-conversion -I${IOS_SIM_GAMBIT_HOME}/include -D___LIBRARY

IOS_SIM_LDFLAGS_LIB=-static -arch_only ${IOS_SIM_ARCH} -syslibroot ${IOS_SIM_SYSLIBROOT=} -ObjC -framework Foundation -L${IOS_SIM_GAMBIT_HOME}/lib -lgambc -o ${IOS_SIM_BUILD_DIR}/${IOS_SIM_LIBRARY}

# ----------------------------------------
# Bard Common files
# ----------------------------------------

SCHEME_SOURCES= \
         src/version.scm \
         lib/uuid.scm \
         lib/srfi101.scm \
         lib/wttree.scm \
         src/util/general.scm \
         src/values/types.scm \
         src/values/values.scm


C_SOURCES= \
         src/version.c \
         lib/uuid.c \
         lib/srfi101.c \
         lib/wttree.c \
         src/util/general.c \
         src/values/types.c \
         src/values/values.c

OBJECTS= \
         version.o \
         uuid.o \
         srfi101.o \
         wttree.o \
         general.o \
         types.o \
         values.o


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
         bard_c_api.o \
         bard.o \
         bard_.o 


# ----------------------------------------
# Inputs to the Bard executable
# ----------------------------------------

MAIN_SCHEME_SOURCES= \
         c_api/objc_data.scm \
         c_api/bard_toplevel.scm \
         src/bard.scm \
         src/main.scm

MAIN_C_SOURCES= \
         c_api/objc_data.c \
         c_api/bard_toplevel.c \
         src/bard.c \
         src/main.c \
         src/main_.c

# ----------------------------------------
# make rules
# ----------------------------------------

all: mac_lib mac_main ios_lib
	make tidy

# -------------------
# Housekeeping

clean:
	rm -f ${MAC_BUILD_DIR}/${MAC_EXECUTABLE}
	rm -f ${MAC_BUILD_DIR}/$(MAC_LIBRARY)
	rm -f ${IOS_BUILD_DIR}/$(IOS_LIBRARY)
	rm -f ${IOS_DEVICE_BUILD_DIR}/$(IOS_DEVICE_LIBRARY)
	rm -f ${IOS_SIM_BUILD_DIR}/$(IOS_SIM_LIBRARY)
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${LIB_C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

tidy:
	rm -f ${IOS_DEVICE_BUILD_DIR}/$(IOS_DEVICE_LIBRARY)
	rm -f ${IOS_SIM_BUILD_DIR}/$(IOS_SIM_LIBRARY)
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${LIB_C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

# -------------------
# Bard Library

# Mac

mac_lib: tidy
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${MAC_CC} ${MAC_CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${MAC_LIBTOOL} ${MAC_LDFLAGS_LIB} ${OBJECTS} ${LIB_OBJECTS}

# iOS 

ios_lib: ios_device_lib ios_sim_lib
	lipo ${IOS_DEVICE_BUILD_DIR}/${IOS_DEVICE_LIBRARY} ${IOS_SIM_BUILD_DIR}/${IOS_SIM_LIBRARY} -create -output ${IOS_BUILD_DIR}/${IOS_LIBRARY}

# iOS Device

ios_device_lib: tidy
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${IOS_DEVICE_CC} ${IOS_DEVICE_CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${IOS_DEVICE_LIBTOOL} ${IOS_DEVICE_LDFLAGS_LIB} ${OBJECTS} ${LIB_OBJECTS}

# iOS Simulator

ios_sim_lib: tidy
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${IOS_SIM_CC} ${IOS_SIM_CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${IOS_SIM_LIBTOOL} ${IOS_SIM_LDFLAGS_LIB} ${OBJECTS} ${LIB_OBJECTS}

# -------------------
# Bard Executable

# Mac

mac_main: tidy
	${GSC} -link ${SCHEME_SOURCES} ${MAIN_SCHEME_SOURCES}
	${MAC_CC} ${MAC_CFLAGS_MAIN} ${MAC_LDFLAGS_MAIN} ${C_SOURCES} ${MAIN_C_SOURCES}


