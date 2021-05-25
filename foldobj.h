/*
    $Id: foldobj.h 2651 2021-05-09 19:33:48Z soci $

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
#ifndef FOLDOBJ_H
#define FOLDOBJ_H
#include "obj.h"

extern struct Type *const FOLD_OBJ;

struct Str;

typedef struct Fold {
    Obj v;
    struct Str *repr;
} Fold;

#define Fold(a) OBJ_CAST(Fold, a)

extern Obj *const fold_value;

extern void foldobj_init(void);
extern void foldobj_destroy(void);

static inline Obj *ref_fold(void) {
    fold_value->refcount++; return fold_value;
}

#endif
