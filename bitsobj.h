/*
    $Id: bitsobj.h 3063 2023-08-27 09:19:30Z soci $

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
#ifndef BITSOBJ_H
#define BITSOBJ_H
#include "obj.h"
#include "oper_e.h"

extern struct Type *const BITS_OBJ;

typedef uint32_t bdigit_t;
typedef struct Bits {
    Obj v;
    ssize_t len;
    size_t bits;
    bdigit_t *data;
    union {
        bdigit_t val[2];
        int hash;
    } u;
} Bits;

#define Bits(a) OBJ_CAST(Bits, a)

extern Obj *const null_bits;
extern Obj *const inv_bits;
extern Obj *const bits_value[2];
extern Obj *const ibits_value[2];

extern void bitsobj_init(void);
extern void bitsobj_names(void);
extern void bitsobj_destroy(void);

struct Str;
struct Bytes;

extern MUST_CHECK Obj *bits_calc1(Oper_types, unsigned int);
extern MUST_CHECK Obj *bits_from_hexstr(const uint8_t *, linecpos_t *);
extern MUST_CHECK Obj *bits_from_binstr(const uint8_t *, linecpos_t *);
extern MUST_CHECK Obj *bits_from_str(struct Str *, linepos_t);
extern MUST_CHECK Obj *bits_from_bytes(const struct Bytes *, linepos_t);
extern MUST_CHECK Obj *bits_from_uval(uval_t, unsigned int);
extern MUST_CHECK Obj *bits_from_obj(Obj *, linepos_t);
extern MUST_CHECK Obj *float_from_bits(const Bits *, linepos_t);

#endif
