EXECUTABLE=bard
LIBRARY=libBard.a

GAMBIT_HOME=/usr/local/gambit/macosx

CFLAGS= -O1 -I${GAMBIT_HOME}/include -L${GAMBIT_HOME}/lib -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp -arch i386

GSC=/usr/local/gambit/macosx/bin/gsc

MAIN_SCHEME_SOURCES=src/version.scm lib/uuid.scm lib/srfi101.scm lib/wttree.scm src/util/general.scm src/util/list.scm src/util/sort.scm src/values/type.scm src/values/function.scm src/values/undefined.scm src/values/nothing.scm src/values/character.scm src/values/boolean.scm src/values/number.scm src/values/name.scm src/values/string.scm src/values/cons.scm src/values/primitive-procedure.scm src/values/frame.scm src/protocols/Anything.scm src/protocols/Type.scm src/protocols/Applicable.scm src/protocols/ForeignValue.scm src/protocols/StructureValue.scm src/protocols/PrimitiveValue.scm src/protocols/Undefined.scm src/protocols/List.scm src/protocols/Atom.scm src/protocols/Text.scm src/protocols/Frame.scm src/protocols/Procedure.scm src/protocols/Name.scm src/protocols/Null.scm src/protocols/Number.scm src/protocols/Character.scm src/protocols/Boolean.scm src/protocols/Method.scm src/protocols/Function.scm src/protocols/Keyword.scm src/protocols/Symbol.scm src/protocols/Float.scm src/protocols/Integer.scm src/protocols/Ratio.scm src/protocols/Comparable.scm src/print.scm src/protocols/IOStream.scm src/protocols/As.scm src/prims.scm src/eval/special.scm src/eval/macro.scm src/eval/apply.scm src/eval/env.scm src/eval/eval.scm src/reader/read.scm src/repl/error.scm src/repl/toplevel.scm src/repl/repl.scm src/main.scm

MAIN_C_SOURCES=src/version.c lib/uuid.c lib/srfi101.c lib/wttree.c src/util/general.c src/util/list.c src/util/sort.c src/values/type.c src/values/function.c src/values/undefined.c src/values/nothing.c src/values/character.c src/values/boolean.c src/values/number.c src/values/name.c src/values/string.c src/values/cons.c src/values/primitive-procedure.c src/values/frame.c src/protocols/Anything.c src/protocols/Type.c src/protocols/Applicable.c src/protocols/ForeignValue.c src/protocols/StructureValue.c src/protocols/PrimitiveValue.c src/protocols/Undefined.c src/protocols/List.c src/protocols/Atom.c src/protocols/Text.c src/protocols/Frame.c src/protocols/Procedure.c src/protocols/Name.c src/protocols/Null.c src/protocols/Number.c src/protocols/Character.c src/protocols/Boolean.c src/protocols/Method.c src/protocols/Function.c src/protocols/Keyword.c src/protocols/Symbol.c src/protocols/Float.c src/protocols/Integer.c src/protocols/Ratio.c src/protocols/Comparable.c src/print.c src/protocols/IOStream.c src/protocols/As.c src/prims.c src/eval/special.c src/eval/macro.c src/eval/apply.c src/eval/env.c src/eval/eval.c src/reader/read.c src/repl/error.c src/repl/toplevel.c src/repl/repl.c src/main.c src/main_.c


LIB_SCHEME_SOURCES=src/version.scm lib/uuid.scm lib/srfi101.scm lib/wttree.scm src/util/general.scm src/util/list.scm src/util/sort.scm src/values/type.scm src/values/function.scm src/values/undefined.scm src/values/nothing.scm src/values/character.scm src/values/boolean.scm src/values/number.scm src/values/name.scm src/values/string.scm src/values/cons.scm src/values/primitive-procedure.scm src/values/frame.scm src/protocols/Anything.scm src/protocols/Type.scm src/protocols/Applicable.scm src/protocols/ForeignValue.scm src/protocols/StructureValue.scm src/protocols/PrimitiveValue.scm src/protocols/Undefined.scm src/protocols/List.scm src/protocols/Atom.scm src/protocols/Text.scm src/protocols/Frame.scm src/protocols/Procedure.scm src/protocols/Name.scm src/protocols/Null.scm src/protocols/Number.scm src/protocols/Character.scm src/protocols/Boolean.scm src/protocols/Method.scm src/protocols/Function.scm src/protocols/Keyword.scm src/protocols/Symbol.scm src/protocols/Float.scm src/protocols/Integer.scm src/protocols/Ratio.scm src/protocols/Comparable.scm src/print.scm src/protocols/IOStream.scm src/protocols/As.scm src/prims.scm src/eval/special.scm src/eval/macro.scm src/eval/apply.scm src/eval/env.scm src/eval/eval.scm src/reader/read.scm src/repl/error.scm src/repl/toplevel.scm src/repl/repl.scm 

LIB_C_SOURCES=src/version.c lib/uuid.c lib/srfi101.c lib/wttree.c src/util/general.c src/util/list.c src/util/sort.c src/values/type.c src/values/function.c src/values/undefined.c src/values/nothing.c src/values/character.c src/values/boolean.c src/values/number.c src/values/name.c src/values/string.c src/values/cons.c src/values/primitive-procedure.c src/values/frame.c src/protocols/Anything.c src/protocols/Type.c src/protocols/Applicable.c src/protocols/ForeignValue.c src/protocols/StructureValue.c src/protocols/PrimitiveValue.c src/protocols/Undefined.c src/protocols/List.c src/protocols/Atom.c src/protocols/Text.c src/protocols/Frame.c src/protocols/Procedure.c src/protocols/Name.c src/protocols/Null.c src/protocols/Number.c src/protocols/Character.c src/protocols/Boolean.c src/protocols/Method.c src/protocols/Function.c src/protocols/Keyword.c src/protocols/Symbol.c src/protocols/Float.c src/protocols/Integer.c src/protocols/Ratio.c src/protocols/Comparable.c src/print.c src/protocols/IOStream.c src/protocols/As.c src/prims.c src/eval/special.c src/eval/macro.c src/eval/apply.c src/eval/env.c src/eval/eval.c src/reader/read.c src/repl/error.c src/repl/toplevel.c src/repl/repl.c src/repl/repl_.c

LIBRARY_OBJECTS=version.o uuid.o srfi101.o wttree.o general.o list.o sort.o type.o function.o undefined.o nothing.o character.o boolean.o number.o name.o string.o cons.o primitive-procedure.o frame.o Anything.o Type.o Applicable.o ForeignValue.o StructureValue.o PrimitiveValue.o Undefined.o List.o Atom.o Text.o Frame.o Procedure.o Name.o Null.o Number.o Character.o Boolean.o Method.o Function.o Keyword.o Symbol.o Float.o Integer.o Ratio.o Comparable.o print.o IOStream.o As.o prims.o special.o macro.o apply.o env.o eval.o read.o error.o toplevel.o repl.o repl_.o


all: main

clean:
	find . -name "*.c" -delete
	find . -name "*.o" -delete

pristine:
	find . -name "*.c" -delete
	find . -name "*.o" -delete
	rm -f ${EXECUTABLE}
	rm -f $(LIBRARY)

main: compile_main_scheme
	gcc ${CFLAGS} -o ${EXECUTABLE} ${GAMBIT_HOME}/lib/libgambc.a ${MAIN_C_SOURCES}

libBard: compile_lib_scheme
	gcc ${CFLAGS} -c  ${LIB_C_SOURCES} ${GAMBIT_HOME}/lib/libgambc.a -D___LIBRARY
	ar rc ${LIBRARY} ${LIBRARY_OBJECTS} && ranlib ${LIBRARY}
	rm -f ${LIB_C_SOURCES}
	rm -f ${LIBRARY_OBJECTS}

compile_main_scheme: 
	${GSC} -link ${MAIN_SCHEME_SOURCES}

compile_lib_scheme: 
	${GSC} -link ${LIB_SCHEME_SOURCES}

