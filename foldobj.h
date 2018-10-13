/*
    $Id: foldobj.h 1600 2018-08-25 16:23:27Z soci $

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

extern struct Type *FOLD_OBJ;

typedef struct Fold {
    Obj v;
    int *dummy;
} Fold;

extern Fold *fold_value;

extern void foldobj_init(void);
extern void foldobj_destroy(void);

static inline Fold *ref_fold(void) {
    fold_value->v.refcount++; return fold_value;
}

#endif
