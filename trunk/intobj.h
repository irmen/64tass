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

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#ifndef _INTOBJ_H
#define _INTOBJ_H
#include "obj.h"
#include "values.h"

extern Type *INT_OBJ;
extern Int *int_value[2];
extern Int *minus1_value;

typedef uint32_t digit_t;
typedef uint64_t twodigits_t;

typedef struct Int {
    Obj v;
    ssize_t len;
    digit_t val[2];
    digit_t *data;
} Int;

extern void intobj_init(void);
extern void intobj_names(void);
extern void intobj_destroy(void);

static inline Int *ref_int(Int *v1) {
    v1->v.refcount++; return v1;
}

static inline MUST_CHECK Int *new_int(void) {
    return (Int *)val_alloc(INT_OBJ);
}

typedef struct Str Str;
typedef struct Bytes Bytes;
typedef struct Bits Bits;
typedef struct Float Float;

extern MUST_CHECK Int *int_from_int(int);
extern MUST_CHECK Int *int_from_size(size_t);
extern MUST_CHECK Int *int_from_uval(uval_t);
extern MUST_CHECK Int *int_from_ival(ival_t);
extern MUST_CHECK Obj *int_from_str(const Str *, linepos_t);
extern MUST_CHECK Int *int_from_bytes(const Bytes *);
extern MUST_CHECK Int *int_from_bits(const Bits *);
extern MUST_CHECK Int *int_from_decstr(const uint8_t *, size_t *);
extern MUST_CHECK Int *int_from_float(const Float *);
extern MUST_CHECK Obj *float_from_int(const Int *, linepos_t);

#endif
