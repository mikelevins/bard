GAMBIT_COMP_HOME=/usr/local/gambit/macosx
GAMBIT_TARGET_HOME=/usr/local/gambit/iphone-simulator
GSC=${GAMBIT_COMP_HOME}/bin/gsc
GSC_INC=${GAMBIT_TARGET_HOME}/include
GSC_COMP_BIN=${GAMBIT_COMP_HOME}/bin
GSC_TARGET_BIN=${GAMBIT_TARGET_HOME}/bin
GSC_COMP_LIB=${GAMBIT_COMP_HOME}/lib
GSC_TARGET_LIB=${GAMBIT_TARGET_HOME}/lib
GCC = /Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/clang -isysroot /Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk
AR = /Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/ar
RANLIB = /Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/ranlib

SCHEME_SOURCES = scm/bardvm.scm

C_SOURCES = scm/bardvm.c scm/bardvm_.c

OBJECTS = bardvm.o bardvm_.o

PRODUCT = libBard.a

FLAGS_OBJ = -arch i386   -Wno-unused -O1 -fno-math-errno -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common 

lib: obj
	${AR} rc ${PRODUCT} ${OBJECTS} && ${RANLIB} ${PRODUCT}
	rm -f ${C_SOURCES}
	rm -f ${OBJECTS}

obj: compile_scheme
	${GCC} ${FLAGS_OBJ} -I"${GSC_INC}" -c ${C_SOURCES} ${GSC_TARGET_LIB}/libgambc.a  -D___LIBRARY

compile_scheme:
	${GSC} -f -link ${SCHEME_SOURCES}

clean:
	rm -f ${C_SOURCES}
	rm -f ${OBJECTS}
	rm -f *~
