# Products

EXECUTABLE=bard
LIBRARY=libBard.a

# Gambit

GAMBIT_HOME=/usr/local/gambit/macosx

GSC=/usr/local/gambit/macosx/bin/gsc

# C Compiler

CC=/Developer/usr/bin/gcc

CFLAGS_LIB= -O1 -I${GAMBIT_HOME}/include -L${GAMBIT_HOME}/lib -x objective-c -arch i386 -std=gnu99 -Wno-trigraphs -fpascal-strings -O0 -fasm-blocks -gdwarf-2 -D___LIBRARY

CFLAGS_MAIN= -O1 -I${GAMBIT_HOME}/include -L${GAMBIT_HOME}/lib -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp -arch i386

# Bard Common files

SCHEME_SOURCES=src/version.scm lib/uuid.scm lib/srfi101.scm lib/wttree.scm src/util/general.scm src/util/list_utils.scm src/util/sort.scm src/values/val_type.scm src/values/val_function.scm src/values/val_undefined.scm src/values/nothing.scm src/values/val_character.scm src/values/val_boolean.scm src/values/val_number.scm src/values/val_name.scm src/values/string.scm src/values/cons.scm src/values/primitive_procedure.scm src/values/val_frame.scm src/protocols/Anything.scm src/protocols/Type.scm src/protocols/Applicable.scm src/protocols/ForeignValue.scm src/protocols/StructureValue.scm src/protocols/PrimitiveValue.scm src/protocols/Undefined.scm src/protocols/List.scm src/protocols/Atom.scm src/protocols/Text.scm src/protocols/Frame.scm src/protocols/Procedure.scm src/protocols/Name.scm src/protocols/Null.scm src/protocols/Number.scm src/protocols/Character.scm src/protocols/Boolean.scm src/protocols/Method.scm src/protocols/Function.scm src/protocols/Keyword.scm src/protocols/Symbol.scm src/protocols/Float.scm src/protocols/Integer.scm src/protocols/Ratio.scm src/protocols/Comparable.scm src/print.scm src/protocols/IOStream.scm src/protocols/As.scm src/prims.scm src/eval/special.scm src/eval/macro.scm src/eval/apply.scm src/eval/env.scm src/eval/eval.scm src/reader/read.scm src/repl/error.scm src/repl/toplevel.scm 

C_SOURCES=src/version.c lib/uuid.c lib/srfi101.c lib/wttree.c src/util/general.c src/util/list_utils.c src/util/sort.c src/values/val_type.c src/values/val_function.c src/values/val_undefined.c src/values/nothing.c src/values/val_character.c src/values/val_boolean.c src/values/val_number.c src/values/val_name.c src/values/string.c src/values/cons.c src/values/primitive_procedure.c src/values/val_frame.c src/protocols/Anything.c src/protocols/Type.c src/protocols/Applicable.c src/protocols/ForeignValue.c src/protocols/StructureValue.c src/protocols/PrimitiveValue.c src/protocols/Undefined.c src/protocols/List.c src/protocols/Atom.c src/protocols/Text.c src/protocols/Frame.c src/protocols/Procedure.c src/protocols/Name.c src/protocols/Null.c src/protocols/Number.c src/protocols/Character.c src/protocols/Boolean.c src/protocols/Method.c src/protocols/Function.c src/protocols/Keyword.c src/protocols/Symbol.c src/protocols/Float.c src/protocols/Integer.c src/protocols/Ratio.c src/protocols/Comparable.c src/print.c src/protocols/IOStream.c src/protocols/As.c src/prims.c src/eval/special.c src/eval/macro.c src/eval/apply.c src/eval/env.c src/eval/eval.c src/reader/read.c src/repl/error.c src/repl/toplevel.c 

OBJECTS=version.o uuid.o srfi101.o wttree.o general.o list_utils.o sort.o val_type.o val_function.o val_undefined.o nothing.o val_character.o val_boolean.o val_number.o val_name.o string.o cons.o primitive_procedure.o val_frame.o Anything.o Type.o Applicable.o ForeignValue.o StructureValue.o PrimitiveValue.o Undefined.o List.o Atom.o Text.o Frame.o Procedure.o Name.o Null.o Number.o Character.o Boolean.o Method.o Function.o Keyword.o Symbol.o Float.o Integer.o Ratio.o Comparable.o print.o IOStream.o As.o prims.o special.o macro.o apply.o env.o eval.o read.o error.o toplevel.o bard.o 

# Bard Library files

LIB_SCHEME_SOURCES=c_api/bard_api.scm c_api/bard_c_api.scm src/bard.scm 

LIB_C_SOURCES=c_api/bard_api.c c_api/bard_c_api.c src/bard.c src/bard_.c

LIB_OBJECTS=bard_api.o bard_c_api.o bard.o bard_.o

# Bard Executable files

MAIN_SCHEME_SOURCES=src/bard.scm src/main.scm

MAIN_C_SOURCES=src/bard.c src/main.c src/main_.c


# make rules

all: main

# Housekeeping

clean:
	rm -f $(LIBRARY)
	rm -f ${EXECUTABLE}
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${LIB_C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

tidy:
	rm -f ${OBJECTS}
	rm -f ${LIB_OBJECTS}
	rm -f ${C_SOURCES}
	rm -f ${LIBRARY_C_SOURCES}
	rm -f ${MAIN_C_SOURCES}

# Bard Library

lib: bardLib

bardLib: 
	${GSC} -link ${SCHEME_SOURCES} ${LIB_SCHEME_SOURCES}
	${CC} ${CFLAGS_LIB} -c ${C_SOURCES} ${LIB_C_SOURCES}
	ar rc ${LIBRARY} ${OBJECTS} ${LIB_OBJECTS} && ranlib ${LIBRARY}

# Bard Executable

main: 
	${GSC} -link ${SCHEME_SOURCES} ${MAIN_SCHEME_SOURCES}
	${CC} ${CFLAGS_MAIN} -o ${EXECUTABLE} ${GAMBIT_HOME}/lib/libgambc.a ${C_SOURCES} ${MAIN_C_SOURCES}


