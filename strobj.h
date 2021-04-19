/*
    $Id: strobj.h 2596 2021-04-18 18:52:11Z soci $

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
#ifndef STROBJ_H
#define STROBJ_H
#include "obj.h"
#include "stdbool.h"

extern struct Type *const STR_OBJ;

typedef struct Str {
    Obj v;
    size_t len;
    size_t chars;
    uint8_t *data;
    union {
        uint8_t val[16];
        struct {
            size_t max;
            int hash;
        } s;
    } u;
} Str;

#define Str(a) ((Str *)(1 ? (a) : (Obj *)(Str *)(a)))

extern Obj *const null_str;

extern void strobj_init(void);
extern void strobj_names(void);
extern void strobj_destroy(void);

struct str_t;

extern MALLOC Str *new_str(size_t);
extern MALLOC Str *new_str2(size_t);
extern MUST_CHECK Obj *str_from_str(const uint8_t *, linecpos_t *, linepos_t);
extern MUST_CHECK Obj *str_from_obj(Obj *, linepos_t);
extern MUST_CHECK Obj *float_from_str(const Str *, linepos_t);
extern size_t str_quoting(const uint8_t *, size_t, uint8_t *);
extern bool tostr(const struct values_s *, struct str_t *);
#endif
