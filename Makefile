CC = gcc
OBJ = 64tass.o opcodes.o misc.o avl.o
#CFLAGS = -O2 -march=i486 -mcpu=i486 -pipe
#CFLAGS = -Wall -O3 -march=i686 -pipe -fomit-frame-pointer -fno-exceptions
CFLAGS = -O2

all: 64tass README

64tass: $(OBJ)

64tass.o: 64tass.c opcodes.h misc.h

opcodes.o: opcodes.c opcodes.h

misc.o: misc.c misc.h opcodes.h libtree.h

avl.o: libtree.h

README: README.html
	-w3m -dump -no-graph README.html >README

.PHONY: clean strip

clean:
	rm -f $(OBJ) 64tass *~

strip:
	strip -R .note -R .comment -R .note.ABI-tag 64tass
