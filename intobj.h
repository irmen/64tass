/*
    $Id: intobj.h 3063 2023-08-27 09:19:30Z soci $

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
#ifndef INTOBJ_H
#define INTOBJ_H
#include "obj.h"

extern struct Type *const INT_OBJ;

typedef uint32_t digit_t;
typedef uint64_t twodigits_t;

typedef struct Int {
    Obj v;
    ssize_t len;
    digit_t val[2];
    digit_t *data;
} Int;

#define Int(a) OBJ_CAST(Int, a)

extern Obj *const int_value[2];
extern Obj *const minus1_value;

extern void intobj_init(void);
extern void intobj_names(void);
extern void intobj_destroy(void);

struct Str;
struct Bytes;
struct Bits;
struct Float;

extern MUST_CHECK Obj *int_from_size(size_t);
extern MUST_CHECK Obj *int_from_uval(uval_t);
extern MUST_CHECK Obj *int_from_ival(ival_t);
extern MUST_CHECK Obj *int_from_str(struct Str *, linepos_t);
extern MUST_CHECK Obj *int_from_bytes(const struct Bytes *, linepos_t);
extern MUST_CHECK Obj *int_from_bits(const struct Bits *, linepos_t);
extern MUST_CHECK Obj *int_from_decstr(const uint8_t *, linecpos_t *, linecpos_t *);
extern MUST_CHECK Obj *int_from_float(const struct Float *, linepos_t);
extern MUST_CHECK Obj *int_from_obj(Obj *, linepos_t);
extern MUST_CHECK Obj *float_from_int(const Int *, linepos_t);

#endif
