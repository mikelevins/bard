BARD_DIRECTORY      =.
HC      = ghc
HC_OPTS = --make -i$(BARD_DIRECTORY)

SRCS =  bard.hs
OBJS = bard.o

all:
	$(HC) $(HC_OPTS) bard

clean:
	rm -f *.o *.hi *~ bard
