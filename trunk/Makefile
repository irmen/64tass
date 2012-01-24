
.SILENT:

CC = gcc
OBJ = 64tass.o opcodes.o misc.o avl.o my_getopt.o eval.o error.o
LANG = C

# uncomment to build 32bit binary on 64bit machine
#LDFLAGS = -m32

#CFLAGS = -O2 -march=i486 -mcpu=i486 -pipe
#CFLAGS = -Wall -O3 -march=i686 -pipe -fomit-frame-pointer -fno-exceptions
CFLAGS = -O2 -W -Wall -Wextra -g
#CFLAGS = -O2

CFLAGS += $(LDFLAGS)

all: 64tass README

64tass: $(OBJ)
#	gcc $(CFLAGS)  64tass.o opcodes.o misc.o avl.o   -o 64tass

64tass.o: 64tass.c opcodes.h misc.h libtree.h eval.h error.h
avl.o: avl.c libtree.h
error.o: error.c error.h misc.h libtree.h
eval.o: eval.c misc.h libtree.h error.h
misc.o: misc.c libtree.h misc.h opcodes.h getopt.h my_getopt.h error.h
my_getopt.o: my_getopt.c my_getopt.h
opcodes.o: opcodes.c opcodes.h

README: README.html
	-w3m -dump -no-graph README.html >README

.PHONY: clean strip

clean:
	rm -f $(OBJ) 64tass *~

strip:
	strip -R .note -R .comment -R .note.ABI-tag 64tass
