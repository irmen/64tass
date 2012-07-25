CC = gcc
OBJ = 64tass.o opcodes.o misc.o avl.o my_getopt.o eval.o error.o section.o encoding.o ternary.o file.o values.o
LANG = C
CFLAGS = -O2 -W -Wall -Wextra -g
LDFLAGS = -g
CFLAGS += $(LDFLAGS)

.SILENT:

all: 64tass README

64tass: $(OBJ)

64tass.o: 64tass.c opcodes.h misc.h libtree.h values.h eval.h error.h section.h encoding.h file.h
avl.o: avl.c libtree.h
encoding.o: encoding.c encoding.h libtree.h error.h misc.h values.h ternary.h
error.o: error.c error.h misc.h libtree.h values.h
eval.o: eval.c eval.h misc.h libtree.h values.h file.h error.h section.h encoding.h
file.o: file.c file.h libtree.h misc.h values.h error.h
misc.o: misc.c libtree.h misc.h values.h opcodes.h getopt.h my_getopt.h error.h section.h encoding.h file.h
my_getopt.o: my_getopt.c my_getopt.h
opcodes.o: opcodes.c opcodes.h
section.o: section.c section.h libtree.h misc.h values.h error.h
ternary.o: ternary.c ternary.h misc.h libtree.h values.h
values.o: values.c values.h error.h misc.h libtree.h

README: README.html
	-w3m -dump -no-graph README.html >README

.PHONY: clean

clean:
	rm -f $(OBJ) 64tass *~
