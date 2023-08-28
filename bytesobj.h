/*
    $Id: bytesobj.h 3063 2023-08-27 09:19:30Z soci $

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
#ifndef BYTESOBJ_H
#define BYTESOBJ_H
#include "obj.h"

extern struct Type *const BYTES_OBJ;

typedef struct Bytes {
    Obj v;
    ssize_t len;
    uint8_t *data;
    union {
        uint8_t val[16];
        struct {
            size_t max;
            int hash;
        } s;
    } u;
} Bytes;

#define Bytes(a) OBJ_CAST(Bytes, a)

extern Obj *const null_bytes;
extern Obj *const inv_bytes;

extern void bytesobj_init(void);
extern void bytesobj_names(void);
extern void bytesobj_destroy(void);

typedef enum Textconv_types {
    BYTES_MODE_TEXT,
    BYTES_MODE_SHIFT_CHECK,
    BYTES_MODE_SHIFT,
    BYTES_MODE_SHIFTL,
    BYTES_MODE_NULL_CHECK,
    BYTES_MODE_NULL,
    BYTES_MODE_PTEXT
} Textconv_types;

extern MALLOC Bytes *new_bytes(size_t);

struct Str;
struct Bits;

extern MUST_CHECK Obj *bytes_from_uval(uval_t, unsigned int);
extern MUST_CHECK Obj *bytes_from_str(struct Str *, linepos_t, Textconv_types);
extern MUST_CHECK Obj *bytes_from_bits(const struct Bits *, linepos_t);
extern MUST_CHECK Obj *bytes_from_hexstr(const uint8_t *, linecpos_t *, linepos_t);
extern MUST_CHECK Obj *bytes_from_z85str(const uint8_t *, linecpos_t *, linepos_t);
extern MUST_CHECK Obj *bytes_from_obj(Obj *, linepos_t);
extern MUST_CHECK Obj *float_from_bytes(const Bytes *, linepos_t);
#endif
