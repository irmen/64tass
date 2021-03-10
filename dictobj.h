/*
    $Id: dictobj.h 2475 2021-03-07 01:34:55Z soci $

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
#ifndef DICTOBJ_H
#define DICTOBJ_H
#include "obj.h"

extern struct Type *const DICT_OBJ;

struct pair_s {
    int hash;
    Obj *key;
    Obj *data;
};

typedef struct Dict {
    Obj v;
    size_t len;
    struct pair_s *data;
    union {
        struct pair_s val[1];
        struct {
            int hash;
            size_t max;
            size_t mask;
        } s;
    } u;
    Obj *def;
} Dict;

#define Dict(a) ((Dict *)(1 ? (a) : (Obj *)(Dict *)(a)))

extern void dictobj_init(void);
extern void dictobj_names(void);

extern Obj *dictobj_parse(struct values_s *, size_t);
extern MUST_CHECK Obj *dict_sort(Dict *, const size_t *);

#endif
