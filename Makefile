CC = gcc
OBJ = 64tass.o opcodes.o misc.o avl.o my_getopt.o eval.o error.o section.o encoding.o ternary.o file.o values.o variables.o mem.o isnprintf.o
LANG = C
REVISION := $(shell svnversion)
CFLAGS = -O2 -W -Wall -Wextra -DREVISION="\" $(REVISION)\"" -g
LDFLAGS = -g -lm
CFLAGS += $(LDFLAGS)

.SILENT:

all: 64tass README

64tass: $(OBJ)

64tass.o: 64tass.c opcodes.h misc.h libtree.h values.h inttypes.h \
 variables.h eval.h error.h section.h encoding.h file.h mem.h
avl.o: avl.c libtree.h
encoding.o: encoding.c encoding.h libtree.h error.h misc.h values.h \
 inttypes.h variables.h ternary.h
error.o: error.c error.h misc.h libtree.h values.h inttypes.h variables.h
eval.o: eval.c eval.h misc.h libtree.h values.h inttypes.h variables.h \
 file.h error.h section.h encoding.h mem.h isnprintf.h
file.o: file.c file.h libtree.h misc.h values.h inttypes.h variables.h \
 error.h
isnprintf.o: isnprintf.c isnprintf.h values.h inttypes.h misc.h libtree.h \
 variables.h error.h eval.h
mem.o: mem.c mem.h inttypes.h misc.h libtree.h values.h variables.h \
 error.h file.h
misc.o: misc.c misc.h libtree.h values.h inttypes.h variables.h opcodes.h \
 getopt.h my_getopt.h error.h section.h encoding.h file.h
my_getopt.o: my_getopt.c my_getopt.h
opcodes.o: opcodes.c opcodes.h
section.o: section.c section.h libtree.h misc.h values.h inttypes.h \
 variables.h error.h
ternary.o: ternary.c ternary.h misc.h libtree.h values.h inttypes.h \
 variables.h
values.o: values.c values.h inttypes.h error.h misc.h libtree.h \
 variables.h
variables.o: variables.c variables.h values.h inttypes.h libtree.h \
 error.h misc.h

README: README.html
	-w3m -dump -no-graph README.html | sed -e 's/\s\+$$//' >README

.PHONY: clean

clean:
	rm -f $(OBJ) 64tass *~
