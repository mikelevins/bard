EXECUTABLE=bard

CFLAGS= -O2 -I/usr/local/gambit/macosx/include -L/usr/local/gambit/macosx/lib -arch i386

CLIBS= -lgambc

GSC=/usr/local/gambit/macosx/bin/gsc

SCHEME_SOURCES=src/version.scm lib/uuid.scm lib/srfi101.scm lib/wttree.scm src/util/general.scm src/util/list.scm src/util/sort.scm src/values/type.scm src/values/function.scm src/values/undefined.scm src/values/nothing.scm src/values/character.scm src/values/boolean.scm src/values/number.scm src/values/name.scm src/values/string.scm src/values/cons.scm src/values/primitive-procedure.scm src/values/frame.scm src/protocols/Anything.scm src/protocols/Type.scm src/protocols/Applicable.scm src/protocols/ForeignValue.scm src/protocols/StructureValue.scm src/protocols/PrimitiveValue.scm src/protocols/Undefined.scm src/protocols/List.scm src/protocols/Atom.scm src/protocols/Text.scm src/protocols/Frame.scm src/protocols/Procedure.scm src/protocols/Name.scm src/protocols/Null.scm src/protocols/Number.scm src/protocols/Character.scm src/protocols/Boolean.scm src/protocols/Method.scm src/protocols/Function.scm src/protocols/Keyword.scm src/protocols/Symbol.scm src/protocols/Float.scm src/protocols/Integer.scm src/protocols/Ratio.scm src/protocols/Comparable.scm src/print.scm src/protocols/IOStream.scm src/protocols/As.scm src/prims.scm src/eval/special.scm src/eval/macro.scm src/eval/apply.scm src/eval/env.scm src/eval/eval.scm src/reader/read.scm src/repl/error.scm src/repl/toplevel.scm src/repl/repl.scm src/main.scm

C_SOURCES=src/version.c lib/uuid.c lib/srfi101.c lib/wttree.c src/util/general.c src/util/list.c src/util/sort.c src/values/type.c src/values/function.c src/values/undefined.c src/values/nothing.c src/values/character.c src/values/boolean.c src/values/number.c src/values/name.c src/values/string.c src/values/cons.c src/values/primitive-procedure.c src/values/frame.c src/protocols/Anything.c src/protocols/Type.c src/protocols/Applicable.c src/protocols/ForeignValue.c src/protocols/StructureValue.c src/protocols/PrimitiveValue.c src/protocols/Undefined.c src/protocols/List.c src/protocols/Atom.c src/protocols/Text.c src/protocols/Frame.c src/protocols/Procedure.c src/protocols/Name.c src/protocols/Null.c src/protocols/Number.c src/protocols/Character.c src/protocols/Boolean.c src/protocols/Method.c src/protocols/Function.c src/protocols/Keyword.c src/protocols/Symbol.c src/protocols/Float.c src/protocols/Integer.c src/protocols/Ratio.c src/protocols/Comparable.c src/print.c src/protocols/IOStream.c src/protocols/As.c src/prims.c src/eval/special.c src/eval/macro.c src/eval/apply.c src/eval/env.c src/eval/eval.c src/reader/read.c src/repl/error.c src/repl/toplevel.c src/repl/repl.c src/main.c src/main_.c


all: bard

clean:
	find . -name *.c -delete

bard: scheme
	gcc ${CFLAGS} -o ${EXECUTABLE} ${CLIBS} ${C_SOURCES}

scheme: 
	${GSC} -link ${SCHEME_SOURCES}

