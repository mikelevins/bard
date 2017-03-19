PROGRAM = bard

SOURCES = cell.c bard.c
OBJECTS = cell.o bard.o

all: $(PROGRAM)

$(PROGRAM): $(OBJECTS)
	gcc -O2 -o $(PROGRAM) $(OBJECTS)

js:
	emcc -O2 -o $(PROGRAM).js $(SOURCES)

html:
	emcc -O2 -o $(PROGRAM).html $(SOURCES)

wasm:
	emcc -O2 -s WASM=1 -o $(PROGRAM).js $(SOURCES)

clean:
	rm -f *.o $(PROGRAM)
	rm -f *.html
	rm -f *.js
	rm -f *.js.mem
	rm -f *.wasm
