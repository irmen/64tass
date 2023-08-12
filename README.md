
# 64tass

64tass is a multi pass optimizing macro assembler for the 65xx series of
processors.

## Installation

Executables for released versions are not difficult to come by.

On Linux there's a good chance it is already included in the package manager of
the distribution. It's also in ports for the BSDs. For Mac it's in the ports or
brew. If the packaged version is not up to date compilation from source is
always an option.

For Windows 64tass is packaged in MSYS2 for a variety of target architectures
including Aarch64. For old versions of Windows (down to XP running on a 686)
Sourceforge hosts binaries. Manual compilation for even older Windows versions
or 32 bit DOS should be possible for those who dare.

## Compilation

The only dependencies are a working C compiler and Make.

The Makefile was written to support both GNU Make and BSD Make.

GCC or Clang can compile it without a problem. Other C compilers may work but
compile flags might need adjustments.

The regular "Makefile" is for producing binaries for the host platform. So
running "make" should build a "64tass" binary. I've heard that might not always
work on OSX where instead make CPPFLAGS="-D_XOPEN_SOURCE" might be needed.

There's a "Makefile.win" for cross compiling with Mingw. The name of the
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

I saw that there exists a VSCode extension as well. Last time I've checked it
didn't display warning messages and so may need manual tweaking at source
level.

Relaunch64 directly supports 64tass and can even do code completion for simple
projects.

64tass is normally used as a part of a build system. For example it's invoked
from a Makefile, script or batch file. Some text editors may be configured to
invoke binary for compilation which might be handy.

Diagnostic messages are printed to console by default but may be saved to a
file. There are various output formats for the results.

## Support

Bug reports and feature requests:

https://sourceforge.net/p/tass64/bugs/
https://sourceforge.net/p/tass64/feature-requests/

The current reference manual is in the README and README.html files. For the
last release it can be read online at:

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

