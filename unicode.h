/*
    $Id: unicode.h 2933 2022-12-23 10:52:39Z soci $

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

extern unsigned int codepage;

struct ubuff_s {
    unichar_t *data;
    uint32_t len, p;
};

struct str_t;

extern FAST_CALL unsigned int utf8in(const uint8_t *, unichar_t *);
extern FAST_CALL unsigned int utf8out(unichar_t, uint8_t *);
extern MUST_CHECK bool extend_ubuff(struct ubuff_s *);
extern MUST_CHECK bool unfc(struct ubuff_s *);
extern MUST_CHECK bool unfkc(struct str_t *, const struct str_t *, int);
extern size_t argv_print(const char *, FILE *);
extern size_t makefile_print(const char *, FILE *);
extern void printable_print(const uint8_t *, FILE *);
extern size_t printable_print2(const uint8_t *, FILE *, size_t);
extern void caret_print(const uint8_t *, FILE *, size_t);
extern size_t calcpos(const uint8_t *, size_t);
extern unichar_t fromiso2(unichar_t);
extern MUST_CHECK wchar_t *utf8_to_wchar(const char *, size_t);
extern uint8_t *char_to_utf8(const char *);
extern FILE *fopen_utf8(const char *, const char *);
extern const char *unicode_character_name(unichar_t);
extern void unicode_init(void);

static inline bool dash_name(const char *name) {
    return (name[0] == '-' && name[1] == 0);
}

static inline unsigned int utf8len(unichar_t ch) {
    if (ch < 0x80) return 1;
    if (ch < 0xe0) return 2;
    if (ch < 0xf0) return 3;
    if (ch < 0xf8) return 4;
    if (ch < 0xfc) return 5;
    return 6;
}
#endif
