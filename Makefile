# ----------------------------------------
# Products
# ----------------------------------------

# Mac

MAC_EXECUTABLE=bard
MAC_LIBRARY=libBard.a

# iOS 

IOS_LIBRARY=libBard.a

# iOS Device

IOS_DEVICE_LIBRARY=libBard_ios.a

# iOS Simulator

IOS_SIM_LIBRARY=libBard_ios_sim.a

# ----------------------------------------
# output directories
# ----------------------------------------

# Mac

MAC_BUILD_DIR=builds/mac

# iOS

IOS_BUILD_DIR=builds/ios

# iOS device

IOS_DEVICE_BUILD_DIR=builds/ios/device

# iOS Simulator

IOS_SIM_BUILD_DIR=builds/ios/simulator

# ----------------------------------------
# Gambit
# ----------------------------------------
# we use the same host GSC to compile
# Scheme sources for all targets

GSC=/usr/local/gambit/macosx/bin/gsc

# Mac

MAC_GAMBIT_HOME=/usr/local/gambit/macosx

# iOS

IOS_GAMBIT_HOME=/usr/local/gambit/iphone

# iOS Simulator

IOS_SIM_GAMBIT_HOME=/usr/local/gambit/iphone-simulator

# ----------------------------------------
# C Compiler
# ----------------------------------------

# Mac

MAC_CC=/Developer/usr/bin/gcc

MAC_CFLAGS_LIB= -O1 -I${MAC_GAMBIT_HOME}/include -L${MAC_GAMBIT_HOME}/lib -x objective-c -framework Foundation -arch i386 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -fasm-blocks -gdwarf-2 -D___LIBRARY

MAC_CFLAGS_MAIN= -O1 -I${MAC_GAMBIT_HOME}/include -L${MAC_GAMBIT_HOME}/lib -x objective-c -framework Foundation -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp -arch i386

# iOS

IOS_CC=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/arm-apple-darwin10-llvm-gcc-4.2 -isysroot /Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.0.sdk

IOS_CFLAGS_LIB= -I${IOS_GAMBIT_HOME}/include -L${IOS_GAMBIT_HOME}/lib -x objective-c  -framework Foundation -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -D___LIBRARY

# iOS Simulator

IOS_SIM_CC=/Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/clang -isysroot /Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk

IOS_SIM_CFLAGS_LIB= -I${IOS_SIM_GAMBIT_HOME}/include -L${IOS_SIM_GAMBIT_HOME}/lib -x objective-c -framework Foundation -arch i386   -Wno-unused -O1 -fno-math-errno -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common  -D___LIBRARY

# ----------------------------------------
# Library tools
# ----------------------------------------

# Mac

MAC_AR=/Developer/usr/bin/ar
MAC_RANLIB = /Developer/usr/bin/ranlib

# iOS

IOS_AR=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/ar
IOS_RANLIB=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/ranlib

# iOS Simulator

IOS_SIM_AR=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/ar
IOS_SIM_RANLIB=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/ranlib

# ----------------------------------------
# Bard Common files
# ----------------------------------------

SCHEME_SOURCES=src/version.scm lib/uuid.scm src/util/general.scm src/util/list_utils.scm src/util/sort.scm src/values/val_type.scm src/values/val_function.scm src/values/val_undefined.scm src/values/nothing.scm src/values/val_character.scm src/values/val_boolean.scm src/values/val_number.scm src/values/val_name.scm src/values/string.scm src/values/cons.scm src/values/primitive_procedure.scm src/values/val_frame.scm src/protocols/Anything.scm src/protocols/Type.scm src/protocols/Applicable.scm src/protocols/ForeignValue.scm src/protocols/StructureValue.scm src/protocols/PrimitiveValue.scm src/protocols/Undefined.scm src/protocols/List.scm src/protocols/Atom.scm src/protocols/Text.scm src/protocols/Frame.scm src/protocols/Procedure.scm src/protocols/Name.scm src/protocols/Null.scm src/protocols/Number.scm src/protocols/Character.scm src/protocols/Boolean.scm src/protocols/Method.scm src/protocols/Function.scm src/protocols/Keyword.scm src/protocols/Symbol.scm src/protocols/Float.scm src/protocols/Integer.scm src/protocols/Ratio.scm src/protocols/Comparable.scm src/print.scm src/protocols/IOStream.scm src/protocols/As.scm src/prims.scm src/eval/special.scm src/eval/macro.scm src/eval/apply.scm src/eval/env.scm src/eval/eval.scm src/reader/read.scm src/repl/error.scm src/repl/toplevel.scm nelson/Puzzle.scm

C_SOURCES=src/version.c lib/uuid.c src/util/general.c src/util/list_utils.c src/util/sort.c src/values/val_type.c src/values/val_function.c src/values/val_undefined.c src/values/nothing.c src/values/val_character.c src/values/val_boolean.c src/values/val_number.c src/values/val_name.c src/values/string.c src/values/cons.c src/values/primitive_procedure.c src/values/val_frame.c src/protocols/Anything.c src/protocols/Type.c src/protocols/Applicable.c src/protocols/ForeignValue.c src/protocols/StructureValue.c src/protocols/PrimitiveValue.c src/protocols/Undefined.c src/protocols/List.c src/protocols/Atom.c src/protocols/Text.c src/protocols/Frame.c src/protocols/Procedure.c src/protocols/Name.c src/protocols/Null.c src/protocols/Number.c src/protocols/Character.c src/protocols/Boolean.c src/protocols/Method.c src/protocols/Function.c src/protocols/Keyword.c src/protocols/Symbol.c src/protocols/Float.c src/protocols/Integer.c src/protocols/Ratio.c src/protocols/Comparable.c src/print.c src/protocols/IOStream.c src/protocols/As.c src/prims.c src/eval/special.c src/eval/macro.c src/eval/apply.c src/eval/env.c src/eval/eval.c src/reader/read.c src/repl/error.c src/repl/toplevel.c nelson/Puzzle.c

OBJECTS=version.o uuid.o general.o list_utils.o sort.o val_type.o val_function.o val_undefined.o nothing.o val_character.o val_boolean.o val_number.o val_name.o string.o cons.o primitive_procedure.o val_frame.o Anything.o Type.o Applicable.o ForeignValue.o StructureValue.o PrimitiveValue.o Undefined.o List.o Atom.o Text.o Frame.o Procedure.o Name.o Null.o Number.o Character.o Boolean.o Method.o Function.o Keyword.o Symbol.o Float.o Integer.o Ratio.o Comparable.o print.o IOStream.o As.o prims.o special.o macro.o apply.o env.o eval.o read.o error.o toplevel.o Puzzle.o

# ----------------------------------------
# Inputs to the Bard library
# ----------------------------------------

LIB_SCHEME_SOURCES=c_api/bard_api.scm c_api/bard_c_api.scm c_api/objc_data.scm c_api/bard_toplevel.scm src/bard.scm 

LIB_C_SOURCES=c_api/bard_api.c c_api/bard_c_api.c c_api/objc_data.c c_api/bard_toplevel.c src/bard.c src/bard_.c

LIB_OBJECTS=bard_api.o bard_c_api.o objc_data.o bard_toplevel.o bard.o bard_.o

# ----------------------------------------
# Inputs to the Bard executable
# ----------------------------------------

MAIN_SCHEME_SOURCES=c_api/objc_data.scm c_api/bard_toplevel.scm src/bard.scm src/main.scm

MAIN_C_SOURCES=c_api/objc_data.c c_api/bard_toplevel.c src/bard.c src/main.c src/main_.c

# ----------------------------------------
# make rules
# ----------------------------------------

all: mac_main

# -------------------
# Housekeeping

clean:
	rm -f ${MAC_BUILD_DIR}/$(MAC_LIBRARY)
	rm -f ${IOS_BUILD_DIR}/$(IOS_LIBRARY)
	rm -f ${IOS_DEVICE_BUILD_DIR}/$(IOS_DEVICE_LIBRARY)
	rm -f ${IOS_SIM_BUILD_DIR}/$(IOS_SIM_LIBRARY)
	rm -f ${MAC_BUILD_DIR}/${MAC_EXECUTABLE}
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${LIB_C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

tidy:
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${LIB_C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

# -------------------
# Bard Library

# Mac

mac_lib:
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${MAC_CC} ${MAC_CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${MAC_AR} rc ${MAC_LIBRARY} ${OBJECTS} ${LIB_OBJECTS} && ${MAC_RANLIB} ${MAC_LIBRARY}
	mv ${MAC_LIBRARY} ${MAC_BUILD_DIR}/${MAC_LIBRARY}

# iOS 

ios_lib: ios_device_lib ios_sim_lib
	lipo ${IOS_DEVICE_BUILD_DIR}/${IOS_DEVICE_LIBRARY} ${IOS_SIM_BUILD_DIR}/${IOS_SIM_LIBRARY} -create -output ${IOS_BUILD_DIR}/${IOS_LIBRARY}

# iOS Device

ios_device_lib:
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${IOS_CC} ${IOS_CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${IOS_AR} rc ${IOS_DEVICE_LIBRARY} ${OBJECTS} ${LIB_OBJECTS} && ${IOS_RANLIB} ${IOS_DEVICE_LIBRARY}
	mv ${IOS_DEVICE_LIBRARY} ${IOS_DEVICE_BUILD_DIR}/${IOS_DEVICE_LIBRARY}

# iOS Simulator

ios_sim_lib:
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${IOS_SIM_CC} ${IOS_SIM_CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	${IOS_SIM_AR} rc ${IOS_SIM_LIBRARY} ${OBJECTS} ${LIB_OBJECTS} && ${IOS_SIM_RANLIB} ${IOS_SIM_LIBRARY}
	mv ${IOS_SIM_LIBRARY} ${IOS_SIM_BUILD_DIR}/${IOS_SIM_LIBRARY}

# -------------------
# Bard Executable

# Mac

mac_main: 
	${GSC} -link ${SCHEME_SOURCES} ${MAIN_SCHEME_SOURCES}
	${MAC_CC} ${MAC_CFLAGS_MAIN} -o ${MAC_EXECUTABLE} -L${MAC_GAMBIT_HOME}/lib/ -lgambc ${C_SOURCES} ${MAIN_C_SOURCES}
	mv ${MAC_EXECUTABLE} ${MAC_BUILD_DIR}/${MAC_EXECUTABLE}


