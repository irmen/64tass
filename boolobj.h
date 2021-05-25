/*
    $Id: boolobj.h 2676 2021-05-20 21:16:34Z soci $

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
#ifndef BOOLOBJ_H
#define BOOLOBJ_H
#include "obj.h"
#include "stdbool.h"

extern struct Type *const BOOL_OBJ;

struct Str;

typedef struct Bool {
    Obj v;
    struct Str *repr;
    bool value;
} Bool;

#define Bool(a) OBJ_CAST(Bool, a)

extern Obj *const true_value;
extern Obj *const false_value;
extern Obj *const bool_value[2];

extern void boolobj_init(void);
extern void boolobj_names(void);
extern void boolobj_destroy(void);

static inline Obj *ref_true() {
    true_value->refcount++; return true_value;
}

static inline Obj *ref_false() {
    false_value->refcount++; return false_value;
}

static inline MUST_CHECK Obj *truth_reference(bool i) {
    return val_reference(bool_value[i ? 1 : 0]);
}

extern MUST_CHECK Obj *int_from_bool(const struct Bool *);
extern MUST_CHECK Obj *float_from_bool(const struct Bool *);
#endif
