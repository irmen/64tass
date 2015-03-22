/*
    $Id$

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
#ifndef _BYTESOBJ_H
#define _BYTESOBJ_H
#include "obj.h"
#include "values.h"

extern struct Type *BYTES_OBJ;

typedef struct Bytes {
    Obj v;
    ssize_t len;
    uint8_t *data;
    uint8_t val[16];
} Bytes;

extern Bytes *null_bytes;
extern Bytes *inv_bytes;

extern void bytesobj_init(void);
extern void bytesobj_names(void);
extern void bytesobj_destroy(void);

enum bytes_mode_e {
    BYTES_MODE_TEXT,
    BYTES_MODE_SHIFT_CHECK,
    BYTES_MODE_SHIFT,
    BYTES_MODE_SHIFTL,
    BYTES_MODE_NULL_CHECK,
    BYTES_MODE_NULL,
    BYTES_MODE_PTEXT
};

static inline Bytes *ref_bytes(Bytes *v1) {
    v1->v.refcount++; return v1;
}

static inline MUST_CHECK Bytes *new_bytes(void) {
    return (Bytes *)val_alloc(BYTES_OBJ);
}

struct Str;

extern MUST_CHECK Bytes *bytes_from_u8(uint8_t);
extern MUST_CHECK Bytes *bytes_from_u16(uint16_t);
extern MUST_CHECK Bytes *bytes_from_uval(uval_t, int);
extern MUST_CHECK Obj *bytes_from_str(const struct Str *, linepos_t, enum bytes_mode_e);
extern MUST_CHECK Obj *float_from_bytes(const Bytes *, linepos_t);
#endif
