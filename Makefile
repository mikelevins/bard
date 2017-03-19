PROGRAM = bard

OBJECTS = cell.o bard.o

all: $(PROGRAM)

$(PROGRAM): $(OBJECTS)
	gcc -O2 -o $(PROGRAM) $(OBJECTS)

clean:
	rm -f *.o $(PROGRAM)
