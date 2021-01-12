OBJS = 64tass.o opcodes.o str.o avl.o my_getopt.o eval.o error.o section.o \
 encoding.o ternary.o file.o values.o variables.o mem.o isnprintf.o macro.o \
 obj.o floatobj.o addressobj.o codeobj.o strobj.o listobj.o boolobj.o bytesobj.o \
 intobj.o bitsobj.o functionobj.o instruction.o unicode.o unicodedata.o listing.o \
 registerobj.o dictobj.o namespaceobj.o operobj.o gapobj.o typeobj.o noneobj.o \
 longjump.o wctype.o wchar.o math.o arguments.o optimizer.o opt_bit.o labelobj.o \
 errorobj.o macroobj.o mfuncobj.o identobj.o memblocksobj.o foldobj.o main.o console.o
LDLIBS = -lm
LANG = C
REVISION != svnversion | grep --color=none "^[1-9]" || echo "2200?"
CFLAGS = -g -O2 -W -Wall -Wextra -Wwrite-strings -Wshadow -fstrict-aliasing -DREVISION="\"$(REVISION)\"" -Wstrict-aliasing=2 -Werror=missing-prototypes
LDFLAGS =
TARGET = 64tass
RM ?= rm -f
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644
prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
datarootdir = $(prefix)/share
mandir = $(datarootdir)/man
man1dir = $(mandir)/man1
docdir = $(datarootdir)/doc/$(TARGET)

.SILENT:

all: $(TARGET) README

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LDLIBS) -o $@

README: README.html
	-command -v w3m >/dev/null 2>/dev/null && sed -e 's/&larr;/<-/g;s/&hellip;/.../g;s/&lowast;/*/g;s/&minus;/-/g;s/&ndash;/-/g;' README.html | w3m -T text/html -dump -no-graph | sed -e 's/\s\+$$//' >README

64tass.o: 64tass.c 64tass.h attributes.h stdbool.h inttypes.h wait_e.h \
 error.h errors_e.h avl.h opcodes.h eval.h values.h section.h str.h \
 encoding.h file.h variables.h macro.h instruction.h unicode.h listing.h \
 optimizer.h arguments.h ternary.h opt_bit.h longjump.h mem.h \
 unicodedata.h listobj.h obj.h codeobj.h strobj.h addressobj.h boolobj.h \
 bytesobj.h oper_e.h intobj.h bitsobj.h functionobj.h namespaceobj.h \
 operobj.h gapobj.h typeobj.h noneobj.h registerobj.h labelobj.h \
 errorobj.h macroobj.h mfuncobj.h memblocksobj.h identobj.h dictobj.h
addressobj.o: addressobj.c addressobj.h obj.h attributes.h inttypes.h \
 values.h stdbool.h error.h errors_e.h avl.h eval.h variables.h str.h \
 arguments.h boolobj.h strobj.h intobj.h operobj.h oper_e.h typeobj.h \
 noneobj.h errorobj.h floatobj.h bitsobj.h bytesobj.h
arguments.o: arguments.c arguments.h stdbool.h inttypes.h 64tass.h \
 attributes.h wait_e.h opcodes.h my_getopt.h file.h avl.h str.h error.h \
 errors_e.h unicode.h wchar.h
avl.o: avl.c avl.h attributes.h stdbool.h
bitsobj.o: bitsobj.c bitsobj.h obj.h attributes.h inttypes.h stdbool.h \
 math.h eval.h variables.h str.h unicode.h encoding.h avl.h errors_e.h \
 error.h arguments.h codeobj.h values.h boolobj.h floatobj.h strobj.h \
 bytesobj.h oper_e.h intobj.h listobj.h operobj.h typeobj.h noneobj.h \
 errorobj.h addressobj.h
boolobj.o: boolobj.c boolobj.h obj.h attributes.h inttypes.h stdbool.h \
 eval.h error.h errors_e.h avl.h variables.h str.h arguments.h floatobj.h \
 values.h strobj.h bytesobj.h oper_e.h bitsobj.h intobj.h operobj.h \
 typeobj.h errorobj.h noneobj.h functionobj.h
bytesobj.o: bytesobj.c bytesobj.h obj.h attributes.h inttypes.h oper_e.h \
 math.h eval.h stdbool.h unicode.h encoding.h avl.h errors_e.h \
 variables.h str.h arguments.h error.h boolobj.h floatobj.h values.h \
 codeobj.h intobj.h strobj.h bitsobj.h listobj.h operobj.h typeobj.h \
 noneobj.h errorobj.h addressobj.h
codeobj.o: codeobj.c codeobj.h obj.h attributes.h inttypes.h values.h \
 eval.h stdbool.h mem.h 64tass.h wait_e.h section.h avl.h str.h \
 variables.h error.h errors_e.h arguments.h boolobj.h floatobj.h \
 namespaceobj.h listobj.h intobj.h bitsobj.h bytesobj.h oper_e.h \
 operobj.h gapobj.h typeobj.h noneobj.h errorobj.h memblocksobj.h \
 identobj.h addressobj.h
console.o: console.c console.h stdbool.h
dictobj.o: dictobj.c dictobj.h obj.h attributes.h inttypes.h eval.h \
 stdbool.h error.h errors_e.h avl.h variables.h str.h intobj.h listobj.h \
 values.h strobj.h boolobj.h operobj.h oper_e.h typeobj.h noneobj.h \
 errorobj.h
encoding.o: encoding.c encoding.h avl.h attributes.h stdbool.h inttypes.h \
 errors_e.h error.h ternary.h unicode.h values.h 64tass.h wait_e.h \
 strobj.h obj.h bytesobj.h oper_e.h typeobj.h errorobj.h str.h
error.o: error.c error.h attributes.h stdbool.h errors_e.h avl.h \
 inttypes.h wchar.h file.h str.h 64tass.h wait_e.h unicode.h eval.h \
 arguments.h opcodes.h section.h macro.h strobj.h obj.h addressobj.h \
 values.h registerobj.h namespaceobj.h operobj.h oper_e.h typeobj.h \
 labelobj.h errorobj.h noneobj.h identobj.h console.h
errorobj.o: errorobj.c errorobj.h obj.h attributes.h inttypes.h \
 errors_e.h str.h stdbool.h eval.h values.h error.h avl.h 64tass.h \
 wait_e.h file.h macro.h typeobj.h registerobj.h namespaceobj.h
eval.o: eval.c eval.h attributes.h inttypes.h stdbool.h math.h section.h \
 avl.h str.h macro.h wait_e.h variables.h 64tass.h unicode.h listing.h \
 error.h errors_e.h values.h arguments.h optimizer.h unicodedata.h \
 floatobj.h obj.h boolobj.h intobj.h bitsobj.h strobj.h codeobj.h \
 bytesobj.h oper_e.h addressobj.h listobj.h dictobj.h registerobj.h \
 namespaceobj.h operobj.h gapobj.h typeobj.h noneobj.h labelobj.h \
 errorobj.h identobj.h foldobj.h memblocksobj.h
file.o: file.c file.h attributes.h stdbool.h inttypes.h avl.h str.h \
 wchar.h 64tass.h wait_e.h unicode.h error.h errors_e.h arguments.h \
 unicodedata.h
floatobj.o: floatobj.c floatobj.h obj.h attributes.h inttypes.h values.h \
 math.h error.h stdbool.h errors_e.h avl.h eval.h variables.h str.h \
 arguments.h boolobj.h codeobj.h strobj.h bytesobj.h oper_e.h intobj.h \
 bitsobj.h operobj.h typeobj.h noneobj.h errorobj.h addressobj.h \
 functionobj.h
foldobj.o: foldobj.c foldobj.h obj.h attributes.h inttypes.h values.h \
 eval.h stdbool.h typeobj.h operobj.h oper_e.h strobj.h errorobj.h \
 errors_e.h str.h
functionobj.o: functionobj.c functionobj.h obj.h attributes.h inttypes.h \
 str.h math.h isnprintf.h eval.h stdbool.h variables.h error.h errors_e.h \
 avl.h file.h arguments.h instruction.h 64tass.h wait_e.h section.h \
 floatobj.h values.h strobj.h listobj.h intobj.h boolobj.h operobj.h \
 oper_e.h typeobj.h noneobj.h errorobj.h bytesobj.h dictobj.h \
 addressobj.h
gapobj.o: gapobj.c gapobj.h obj.h attributes.h inttypes.h eval.h \
 stdbool.h variables.h str.h values.h strobj.h operobj.h oper_e.h \
 intobj.h boolobj.h typeobj.h errorobj.h errors_e.h
identobj.o: identobj.c identobj.h obj.h attributes.h inttypes.h str.h \
 eval.h stdbool.h unicode.h error.h errors_e.h avl.h file.h values.h \
 typeobj.h operobj.h oper_e.h strobj.h
instruction.o: instruction.c instruction.h attributes.h stdbool.h \
 inttypes.h opcodes.h 64tass.h wait_e.h section.h avl.h str.h file.h \
 listing.h error.h errors_e.h longjump.h arguments.h optimizer.h \
 addressobj.h obj.h values.h listobj.h registerobj.h codeobj.h typeobj.h \
 noneobj.h errorobj.h memblocksobj.h
intobj.o: intobj.c intobj.h obj.h attributes.h inttypes.h math.h \
 unicode.h stdbool.h encoding.h avl.h errors_e.h error.h eval.h \
 variables.h str.h arguments.h boolobj.h floatobj.h values.h codeobj.h \
 strobj.h bytesobj.h oper_e.h bitsobj.h operobj.h typeobj.h noneobj.h \
 errorobj.h addressobj.h functionobj.h
isnprintf.o: isnprintf.c isnprintf.h attributes.h inttypes.h unicode.h \
 stdbool.h eval.h error.h errors_e.h avl.h floatobj.h obj.h values.h \
 strobj.h intobj.h typeobj.h noneobj.h errorobj.h str.h addressobj.h
labelobj.o: labelobj.c labelobj.h obj.h attributes.h inttypes.h str.h \
 stdbool.h values.h error.h errors_e.h avl.h unicode.h file.h strobj.h \
 typeobj.h errorobj.h
listing.o: listing.c listing.h attributes.h inttypes.h stdbool.h file.h \
 avl.h str.h error.h errors_e.h 64tass.h wait_e.h opcodes.h unicode.h \
 section.h instruction.h obj.h values.h arguments.h
listobj.o: listobj.c listobj.h obj.h attributes.h inttypes.h values.h \
 stdbool.h eval.h variables.h str.h error.h errors_e.h avl.h arguments.h \
 boolobj.h codeobj.h strobj.h intobj.h operobj.h oper_e.h typeobj.h \
 noneobj.h errorobj.h foldobj.h
longjump.o: longjump.c longjump.h avl.h attributes.h stdbool.h inttypes.h \
 section.h str.h error.h errors_e.h
macro.o: macro.c macro.h inttypes.h wait_e.h stdbool.h file.h \
 attributes.h avl.h str.h eval.h values.h section.h variables.h 64tass.h \
 listing.h error.h errors_e.h arguments.h optimizer.h listobj.h obj.h \
 typeobj.h noneobj.h namespaceobj.h labelobj.h macroobj.h mfuncobj.h \
 memblocksobj.h
macroobj.o: macroobj.c macroobj.h obj.h attributes.h inttypes.h str.h \
 stdbool.h values.h eval.h error.h errors_e.h avl.h file.h operobj.h \
 oper_e.h typeobj.h namespaceobj.h intobj.h noneobj.h errorobj.h
main.o: main.c 64tass.h attributes.h stdbool.h inttypes.h wait_e.h \
 wchar.h error.h errors_e.h avl.h unicode.h console.h
math.o: math.c math.h
memblocksobj.o: memblocksobj.c memblocksobj.h obj.h attributes.h \
 inttypes.h stdbool.h values.h error.h errors_e.h avl.h typeobj.h
mem.o: mem.c mem.h attributes.h stdbool.h inttypes.h error.h errors_e.h \
 avl.h file.h str.h 64tass.h wait_e.h listing.h arguments.h values.h \
 memblocksobj.h obj.h
mfuncobj.o: mfuncobj.c mfuncobj.h obj.h attributes.h inttypes.h str.h \
 stdbool.h values.h eval.h error.h errors_e.h avl.h macro.h wait_e.h \
 file.h typeobj.h namespaceobj.h operobj.h oper_e.h listobj.h
my_getopt.o: my_getopt.c my_getopt.h stdbool.h unicode.h attributes.h \
 inttypes.h error.h errors_e.h avl.h
namespaceobj.o: namespaceobj.c namespaceobj.h obj.h attributes.h \
 inttypes.h variables.h stdbool.h str.h eval.h error.h errors_e.h avl.h \
 arguments.h 64tass.h wait_e.h listobj.h values.h strobj.h operobj.h \
 oper_e.h typeobj.h noneobj.h labelobj.h errorobj.h identobj.h codeobj.h \
 macroobj.h mfuncobj.h
noneobj.o: noneobj.c noneobj.h obj.h attributes.h inttypes.h eval.h \
 stdbool.h values.h typeobj.h errorobj.h errors_e.h str.h
obj.o: obj.c obj.h attributes.h inttypes.h eval.h stdbool.h error.h \
 errors_e.h avl.h values.h boolobj.h floatobj.h strobj.h macroobj.h str.h \
 intobj.h listobj.h namespaceobj.h addressobj.h codeobj.h registerobj.h \
 bytesobj.h oper_e.h bitsobj.h functionobj.h dictobj.h operobj.h gapobj.h \
 typeobj.h noneobj.h labelobj.h errorobj.h mfuncobj.h identobj.h \
 memblocksobj.h foldobj.h
opcodes.o: opcodes.c opcodes.h inttypes.h
operobj.o: operobj.c operobj.h obj.h attributes.h inttypes.h oper_e.h \
 strobj.h stdbool.h typeobj.h
opt_bit.o: opt_bit.c opt_bit.h stdbool.h attributes.h error.h errors_e.h \
 avl.h inttypes.h
optimizer.o: optimizer.c optimizer.h inttypes.h stdbool.h error.h \
 attributes.h errors_e.h avl.h section.h str.h opcodes.h opt_bit.h \
 macro.h wait_e.h 64tass.h
registerobj.o: registerobj.c registerobj.h obj.h attributes.h inttypes.h \
 stdbool.h eval.h variables.h str.h values.h boolobj.h strobj.h intobj.h \
 operobj.h oper_e.h typeobj.h errorobj.h errors_e.h
section.o: section.c section.h avl.h attributes.h stdbool.h str.h \
 inttypes.h unicode.h error.h errors_e.h 64tass.h wait_e.h values.h \
 intobj.h obj.h longjump.h optimizer.h eval.h mem.h memblocksobj.h
str.o: str.c str.h inttypes.h unicode.h attributes.h stdbool.h error.h \
 errors_e.h avl.h arguments.h
strobj.o: strobj.c strobj.h obj.h attributes.h inttypes.h stdbool.h \
 eval.h unicode.h error.h errors_e.h avl.h variables.h str.h arguments.h \
 boolobj.h bytesobj.h oper_e.h intobj.h bitsobj.h listobj.h values.h \
 operobj.h typeobj.h noneobj.h errorobj.h
ternary.o: ternary.c ternary.h stdbool.h inttypes.h unicode.h \
 attributes.h error.h errors_e.h avl.h
typeobj.o: typeobj.c typeobj.h obj.h attributes.h inttypes.h stdbool.h \
 variables.h str.h eval.h error.h errors_e.h avl.h strobj.h operobj.h \
 oper_e.h intobj.h boolobj.h listobj.h values.h noneobj.h errorobj.h \
 functionobj.h
unicodedata.o: unicodedata.c unicodedata.h attributes.h inttypes.h
unicode.o: unicode.c unicode.h attributes.h inttypes.h stdbool.h wchar.h \
 wctype.h error.h errors_e.h avl.h unicodedata.h str.h console.h
values.o: values.c values.h attributes.h obj.h inttypes.h unicode.h \
 stdbool.h error.h errors_e.h avl.h strobj.h typeobj.h
variables.o: variables.c variables.h stdbool.h str.h inttypes.h unicode.h \
 attributes.h 64tass.h wait_e.h file.h avl.h obj.h error.h errors_e.h \
 values.h arguments.h eval.h boolobj.h floatobj.h namespaceobj.h strobj.h \
 codeobj.h registerobj.h functionobj.h listobj.h intobj.h bytesobj.h \
 oper_e.h bitsobj.h dictobj.h addressobj.h gapobj.h typeobj.h noneobj.h \
 labelobj.h errorobj.h mfuncobj.h
wchar.o: wchar.c wchar.h inttypes.h
wctype.o: wctype.c wctype.h

.PHONY: all clean distclean install install-strip uninstall install-man install-doc

clean:
	-$(RM) $(OBJS)

distclean: clean
	-$(RM) $(TARGET)

install-man:
	-$(INSTALL) -d $(DESTDIR)$(man1dir)
	-$(INSTALL_DATA) $(TARGET).1 $(DESTDIR)$(man1dir)/$(TARGET).1
	-gzip -9f $(DESTDIR)$(man1dir)/$(TARGET).1

install-doc:
	-$(INSTALL) -d $(DESTDIR)$(docdir)
	-$(INSTALL_DATA) LICENSE-GPL-2.0 LICENSE-LGPL-2.0 LICENSE-LGPL-2.1 LICENSE-my_getopt README.html $(DESTDIR)$(docdir)

install: $(TARGET) install-man install-doc
	-$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(TARGET) $(DESTDIR)$(bindir)/$(TARGET)

install-strip: $(TARGET) install-man install-doc
	-$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) -s $(TARGET) $(DESTDIR)$(bindir)/$(TARGET)

uninstall:
	-$(RM) $(DESTDIR)$(bindir)/$(TARGET)
	-$(RM) $(DESTDIR)$(man1dir)/$(TARGET).1
	-$(RM) $(DESTDIR)$(man1dir)/$(TARGET).1.gz
	-$(RM) $(DESTDIR)$(docdir)/LICENSE-GPL-2.0
	-$(RM) $(DESTDIR)$(docdir)/LICENSE-LGPL-2.0
	-$(RM) $(DESTDIR)$(docdir)/LICENSE-LGPL-2.1
	-$(RM) $(DESTDIR)$(docdir)/LICENSE-my_getopt
	-$(RM) $(DESTDIR)$(docdir)/README.html
