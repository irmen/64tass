/*
    $Id: unicode.h 2550 2021-03-20 01:04:25Z soci $

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
#ifndef UNICODE_H
#define UNICODE_H
#include <stdio.h>
#include "attributes.h"
#include "inttypes.h"
#include "stdbool.h"

struct ubuff_s {
    uchar_t *data;
    uint32_t len, p;
};

struct str_t;

extern FAST_CALL unsigned int utf8in(const uint8_t *, uchar_t *);
extern FAST_CALL unsigned int utf8out(uchar_t, uint8_t *);
extern MUST_CHECK bool extend_ubuff(struct ubuff_s *);
extern MUST_CHECK bool unfc(struct ubuff_s *);
extern MUST_CHECK bool unfkc(struct str_t *, const struct str_t *, int);
extern size_t argv_print(const char *, FILE *);
extern size_t makefile_print(const char *, FILE *);
extern void printable_print(const uint8_t *, FILE *);
extern size_t printable_print2(const uint8_t *, FILE *, size_t);
extern void caret_print(const uint8_t *, FILE *, size_t);
extern size_t calcpos(const uint8_t *, size_t);

static inline unsigned int utf8len(uchar_t ch) {
    if (ch < 0x80) return 1;
    if (ch < 0xe0) return 2;
    if (ch < 0xf0) return 3;
    if (ch < 0xf8) return 4;
    if (ch < 0xfc) return 5;
    return 6;
}
#endif
