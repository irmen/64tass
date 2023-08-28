/*
    $Id: strobj.h 3063 2023-08-27 09:19:30Z soci $

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

#define Str(a) OBJ_CAST(Str, a)

extern Obj *const null_str;

extern void strobj_init(void);
extern void strobj_names(void);
extern void strobj_destroy(void);

struct str_t;

extern MALLOC Str *new_str(size_t);
extern MALLOC Str *new_str2(size_t);
extern uint8_t *extend_str(Str *, size_t);
extern MUST_CHECK Obj *str_from_str(const uint8_t *, linecpos_t *, linepos_t);
extern MUST_CHECK Obj *str_from_obj(Obj *, linepos_t);
extern MUST_CHECK Obj *float_from_str(Str *, linepos_t);
extern size_t str_quoting(const uint8_t *, size_t, uint8_t *);
extern bool tostr(const struct values_s *, struct str_t *);
extern MUST_CHECK Obj *tostr2(const struct values_s *, struct str_t *);
#endif
