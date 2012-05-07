CC = gcc
OBJ = 64tass.o opcodes.o misc.o avl.o my_getopt.o eval.o error.o
LANG = C
CFLAGS = -O2 -W -Wall -Wextra
LDFLAGS = -s
CFLAGS += $(LDFLAGS)

.SILENT:

all: 64tass README

64tass: $(OBJ)

64tass.o: 64tass.c opcodes.h misc.h libtree.h eval.h error.h
avl.o: avl.c libtree.h
error.o: error.c error.h misc.h libtree.h
eval.o: eval.c eval.h misc.h libtree.h error.h
misc.o: misc.c libtree.h misc.h opcodes.h getopt.h my_getopt.h error.h
my_getopt.o: my_getopt.c my_getopt.h
opcodes.o: opcodes.c opcodes.h

README: README.html
	-w3m -dump -no-graph README.html >README

.PHONY: clean

clean:
	rm -f $(OBJ) 64tass *~
