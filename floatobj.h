/*
    $Id: floatobj.h 2651 2021-05-09 19:33:48Z soci $

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
#ifndef FLOATOBJ_H
#define FLOATOBJ_H
#include "obj.h"
#include "values.h"

extern struct Type *const FLOAT_OBJ;

typedef struct Float {
    Obj v;
    double real;
} Float;

#define Float(a) OBJ_CAST(Float, a)

extern void floatobj_init(void);
extern void floatobj_names(void);
extern void floatobj_destroy(void);

static inline MUST_CHECK Obj *new_float(double d) {
    Float *v = Float(val_alloc(FLOAT_OBJ));
    v->real = d;
    return Obj(v);
}

struct oper_s;

extern MUST_CHECK Obj *float_from_double(double, linepos_t);
extern MUST_CHECK Obj *float_from_obj(Obj *, linepos_t);

#endif
