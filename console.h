/*
    $Id: console.h 2950 2023-01-05 21:13:55Z soci $

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

*/
#ifndef CONSOLE_H
#define CONSOLE_H
#ifndef _MSC_VER
#include "unistd.h"
#endif
#include "stdbool.h"

#if _POSIX_C_SOURCE >= 1 || _XOPEN_SOURCE || _POSIX_SOURCE || _POSIX_VERSION || _POSIX2_VERSION || defined _WIN32 || defined __WATCOMC__
#include <stdio.h>

#define COLOR_OUTPUT
extern bool console_use_color;
extern bool console_use_bold;
extern void console_use(FILE *);

#ifdef _WIN32
extern void console_init(unsigned int);
extern void console_destroy(void);
extern void console_attribute(int c, FILE *f);
#define console_bold(f) console_attribute(0, (f))
#define console_defaultbold(f) console_attribute(1, (f))
#define console_reverse(f) console_attribute(2, (f))
#define console_default(f) console_attribute(3, (f))
#define console_cyan(f) console_attribute(4, (f))
#define console_red(f) console_attribute(5, (f))
#define console_boldgreen(f) console_attribute(6, (f))
#define console_purple(f) console_attribute(7, (f))
#else
extern void console_init(void);
#define console_bold(f) fputs("\33[1m", (f))
#define console_defaultbold(f) fputs("\33[0;1m", (f))
#define console_reverse(f) fputs("\33[7m", (f))
#define console_default(f) fputs("\33[m", (f))
#define console_cyan(f) fputs("\33[36m", (f))
#define console_red(f) fputs("\33[31m", (f))
#define console_boldgreen(f) fputs("\33[1;32m", (f))
#define console_purple(f) fputs("\33[35m", (f))
#endif

#else
#define console_init() do {} while (false)
#define console_use_color false
#define console_use_bold false
#define console_use(f) do {} while (false)
#define console_bold(f) do {} while (false)
#define console_defaultbold(f) do {} while (false)
#define console_reverse(f) do {} while (false)
#define console_default(f) do {} while (false)
#define console_cyan(f) do {} while (false)
#define console_red(f) do {} while (false)
#define console_boldgreen(f) do {} while (false)
#define console_purple(f) do {} while (false)
#endif

#endif
