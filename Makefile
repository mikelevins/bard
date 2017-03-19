PROGRAM = bard

SOURCES = cell.c bard.c
OBJECTS = cell.o bard.o

all: $(PROGRAM)

$(PROGRAM): $(OBJECTS)
	gcc -O2 -o $(PROGRAM) $(OBJECTS)

js:
	emcc -o $(PROGRAM).js $(SOURCES)

html:
	emcc -o $(PROGRAM).html $(SOURCES)

clean:
	rm -f *.o $(PROGRAM)
	rm -f *.js
	rm -f *.html
