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

clean:
	rm -f *.o $(PROGRAM)
	rm -f *.js
	rm -f *.html
