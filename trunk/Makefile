
.SILENT:

CC = gcc
OBJ = 64tass.o opcodes.o misc.o avl.o my_getopt.o
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

64tass.o: 64tass.c opcodes.h misc.h

opcodes.o: opcodes.c opcodes.h

misc.o: misc.c misc.h opcodes.h libtree.h getopt.h

avl.o: libtree.h

my_getopt.o: my_getopt.c getopt.h my_getopt.h

README: README.html
	-w3m -dump -no-graph README.html >README

.PHONY: clean strip

clean:
	rm -f $(OBJ) 64tass *~

strip:
	strip -R .note -R .comment -R .note.ABI-tag 64tass
