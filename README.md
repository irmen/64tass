
# 64tass

64tass is a multi pass optimizing macro assembler for the 65xx series of
processors.

## Installation

The Makefile was written to support both GNU Make and BSD Make.

The code was written in C and GCC or Clang can compile it without a problem.
Other C compilers may work but compile flags might need adjustments.

The regular "Makefile" is for producing binaries for the host platform. So
running "make" should build a "64tass" binary. I've heard that might not always
work on OSX where instead make CPPFLAGS="-D_XOPEN_SOURCE" might be needed.

There's a "Makefile.win" for cross compiling using Mingw. The name of the
compiler needs to be adjusted if it's not the same what I use. Of course one
might try to use MSVC natively and there's a good chance it will work. However
that's not actively tested and needs to be reported in case it's accidentally
broken.

There's a "Makefile.amigaos" for cross compiling as well. I use it for cross
compiling with VBCC but it might also work in non cross compile situations.

### Makefile targets

If no target is given it just compiles the binary. Other useful targets:

- clean: deletes all objects
- distclean: delete the binary as well

These targets assume that "prefix" is set otherwise the destination will be
/usr/local. It's recommended to check what these targets actually do to avoid
surprises.

- install-man: installs the man file man1
- install-doc: installs the reference manual to doc dir
- install: install everything, with symbols
- install-strip: install everything, without symbols
- uninstall: remove what was installed

## Usage

This document would become way too long if I'd go into details so please check
the reference manual!

As it's a command line assembler the source files are edited in a text editor.
Sample syntax highlight configuration files are included for these editors:

- Crimson editor
- Gedit
- Jedit
- Kate
- Mcedit
- Notepad++
- PSPad
- Sublime
- VIM

I saw that there exists a VSCode extension as well.

## Support

Bug reports and feature requests:

https://sourceforge.net/p/tass64/bugs/
https://sourceforge.net/p/tass64/feature-requests/

The current reference manual is in the README and README.html files but can be
also read online at:

https://tass64.sourceforge.net/

## Contributing

Development is done at:

https://sourceforge.net/p/tass64

SVN repository:

https://svn.code.sf.net/p/tass64/code/trunk

## License

GNU GENERAL PUBLIC LICENSE Version 2, June 1991

## Project status

It is actively maintained. Release frequency might vary and might be too long
for some. If so please try to compile the current repository version. It might
have bug fixes already but on the other hand it might have new problems.

