SHELL = /bin/sh
CC = gcc
OBJ = 64tass.o opcodes.o misc.o avl.o my_getopt.o eval.o error.o section.o encoding.o ternary.o file.o values.o variables.o mem.o isnprintf.o macro.o obj.o floatobj.o addressobj.o codeobj.o strobj.o listobj.o boolobj.o bytesobj.o intobj.o bitsobj.o functionobj.o instruction.o unicode.o unicodedata.o listing.o registerobj.o dictobj.o
LIBS = -lm
LANG = C
REVISION := $(shell svnversion | grep "^[1-9]" || echo "883?")
CFLAGS = -pipe -O2 -W -Wall -Wextra -Wwrite-strings -Wshadow -fstrict-aliasing -DREVISION="\"$(REVISION)\"" -g -Wstrict-aliasing=2 -Werror=missing-prototypes
LDFLAGS = -g
CFLAGS += $(LDFLAGS)
TARGET = 64tass
PREFIX = $(DESTDIR)/usr/local
BINDIR = $(PREFIX)/bin

.SILENT:

all: $(TARGET) README

$(TARGET): $(OBJ)
	$(CC) -o $@ $^ $(LIBS)

64tass.o: 64tass.c 64tass.h inttypes.h opcodes.h misc.h eval.h values.h \
 error.h libtree.h obj.h intobj.h bitsobj.h bytesobj.h strobj.h \
 registerobj.h listobj.h codeobj.h addressobj.h functionobj.h dictobj.h \
 section.h mem.h encoding.h file.h variables.h macro.h instruction.h \
 unicode.h unicodedata.h listing.h floatobj.h boolobj.h
addressobj.o: addressobj.c values.h inttypes.h error.h libtree.h obj.h \
 intobj.h bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h boolobj.h
avl.o: avl.c libtree.h
bitsobj.o: bitsobj.c values.h inttypes.h error.h libtree.h obj.h intobj.h \
 bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h eval.h unicode.h unicodedata.h \
 encoding.h boolobj.h floatobj.h
boolobj.o: boolobj.c values.h inttypes.h error.h libtree.h obj.h intobj.h \
 bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h boolobj.h floatobj.h
bytesobj.o: bytesobj.c values.h inttypes.h error.h libtree.h obj.h \
 intobj.h bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h eval.h unicode.h unicodedata.h \
 encoding.h boolobj.h floatobj.h
codeobj.o: codeobj.c values.h inttypes.h error.h libtree.h obj.h intobj.h \
 bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h eval.h mem.h 64tass.h opcodes.h \
 section.h variables.h boolobj.h floatobj.h
dictobj.o: dictobj.c values.h inttypes.h error.h libtree.h obj.h intobj.h \
 bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h eval.h
encoding.o: encoding.c encoding.h libtree.h inttypes.h error.h ternary.h \
 misc.h values.h obj.h intobj.h bitsobj.h bytesobj.h strobj.h \
 registerobj.h listobj.h codeobj.h addressobj.h functionobj.h dictobj.h \
 64tass.h opcodes.h unicode.h unicodedata.h
error.o: error.c error.h inttypes.h libtree.h misc.h values.h obj.h \
 intobj.h bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h file.h variables.h 64tass.h \
 opcodes.h macro.h unicode.h unicodedata.h
eval.o: eval.c eval.h values.h inttypes.h error.h libtree.h obj.h \
 intobj.h bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h file.h section.h mem.h encoding.h \
 macro.h variables.h 64tass.h opcodes.h misc.h unicode.h unicodedata.h \
 listing.h floatobj.h boolobj.h
file.o: file.c file.h inttypes.h libtree.h values.h error.h obj.h \
 intobj.h bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h misc.h 64tass.h opcodes.h unicode.h \
 unicodedata.h
floatobj.o: floatobj.c values.h inttypes.h error.h libtree.h obj.h \
 intobj.h bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h floatobj.h boolobj.h
functionobj.o: functionobj.c values.h inttypes.h error.h libtree.h obj.h \
 intobj.h bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h isnprintf.h eval.h variables.h \
 floatobj.h
instruction.o: instruction.c instruction.h inttypes.h opcodes.h values.h \
 error.h libtree.h obj.h intobj.h bitsobj.h bytesobj.h strobj.h \
 registerobj.h listobj.h codeobj.h addressobj.h functionobj.h dictobj.h \
 64tass.h misc.h section.h mem.h file.h listing.h
intobj.o: intobj.c values.h inttypes.h error.h libtree.h obj.h intobj.h \
 bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h unicode.h unicodedata.h encoding.h \
 boolobj.h floatobj.h
isnprintf.o: isnprintf.c isnprintf.h inttypes.h unicode.h unicodedata.h \
 eval.h values.h error.h libtree.h obj.h intobj.h bitsobj.h bytesobj.h \
 strobj.h registerobj.h listobj.h codeobj.h addressobj.h functionobj.h \
 dictobj.h floatobj.h
listing.o: listing.c listing.h inttypes.h file.h libtree.h error.h \
 64tass.h opcodes.h unicode.h unicodedata.h misc.h section.h mem.h \
 instruction.h obj.h values.h intobj.h bitsobj.h bytesobj.h strobj.h \
 registerobj.h listobj.h codeobj.h addressobj.h functionobj.h dictobj.h
listobj.o: listobj.c values.h inttypes.h error.h libtree.h obj.h intobj.h \
 bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h eval.h boolobj.h
macro.o: macro.c macro.h inttypes.h misc.h file.h libtree.h eval.h \
 values.h error.h obj.h intobj.h bitsobj.h bytesobj.h strobj.h \
 registerobj.h listobj.h codeobj.h addressobj.h functionobj.h dictobj.h \
 section.h mem.h variables.h 64tass.h opcodes.h listing.h
mem.o: mem.c mem.h inttypes.h error.h libtree.h file.h misc.h 64tass.h \
 opcodes.h listing.h
misc.o: misc.c misc.h inttypes.h 64tass.h opcodes.h getopt.h my_getopt.h \
 section.h libtree.h mem.h encoding.h file.h eval.h values.h error.h \
 obj.h intobj.h bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h \
 codeobj.h addressobj.h functionobj.h dictobj.h variables.h ternary.h \
 unicode.h unicodedata.h
my_getopt.o: my_getopt.c my_getopt.h unicode.h inttypes.h unicodedata.h
obj.o: obj.c values.h inttypes.h error.h libtree.h obj.h intobj.h \
 bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h variables.h misc.h section.h mem.h \
 64tass.h opcodes.h eval.h boolobj.h floatobj.h macro.h
opcodes.o: opcodes.c opcodes.h
registerobj.o: registerobj.c values.h inttypes.h error.h libtree.h obj.h \
 intobj.h bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h boolobj.h
section.o: section.c unicode.h inttypes.h unicodedata.h section.h \
 libtree.h mem.h error.h misc.h 64tass.h opcodes.h
strobj.o: strobj.c values.h inttypes.h error.h libtree.h obj.h intobj.h \
 bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h eval.h misc.h unicode.h \
 unicodedata.h boolobj.h floatobj.h
ternary.o: ternary.c ternary.h unicode.h inttypes.h unicodedata.h error.h \
 libtree.h
unicodedata.o: unicodedata.c unicodedata.h
unicode.o: unicode.c unicode.h inttypes.h unicodedata.h error.h libtree.h
values.o: values.c values.h inttypes.h error.h libtree.h obj.h intobj.h \
 bitsobj.h bytesobj.h strobj.h registerobj.h listobj.h codeobj.h \
 addressobj.h functionobj.h dictobj.h boolobj.h unicode.h unicodedata.h
variables.o: variables.c unicode.h inttypes.h unicodedata.h variables.h \
 libtree.h misc.h values.h error.h obj.h intobj.h bitsobj.h bytesobj.h \
 strobj.h registerobj.h listobj.h codeobj.h addressobj.h functionobj.h \
 dictobj.h 64tass.h opcodes.h file.h boolobj.h floatobj.h

README: README.html
	-sed -e 's/&larr;/<-/g;s/&hellip;/.../g;s/&lowast;/*/g;s/&minus;/-/g;s/&ndash;/-/g;' README.html | w3m -T text/html -dump -no-graph | sed -e 's/\s\+$$//' >README


.PHONY: all clean distclean install install-strip uninstall

clean:
	-rm -f $(OBJ)

distclean: clean
	-rm -f $(TARGET)

install: $(TARGET)
	install -D $(TARGET) $(BINDIR)/$(TARGET)

install-strip: $(TARGET)
	install -D -s $(TARGET) $(BINDIR)/$(TARGET)

uninstall:
	-rm $(BINDIR)/$(TARGET)
