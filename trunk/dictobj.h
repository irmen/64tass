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
#ifndef _DICTOBJ_H
#define _DICTOBJ_H
#include "obj.h"
#include "values.h"
#include "libtree.h"

extern struct Type *DICT_OBJ;

typedef struct Dict {
    Obj v;
    size_t len;
    struct avltree members;
    Obj *def;
} Dict;

extern void dictobj_init(void);
extern void dictobj_names(void);

static inline MUST_CHECK Dict *new_dict(void) {
    Dict *v = (Dict *)val_alloc(DICT_OBJ);
    avltree_init(&v->members);
    return v;
}

struct pair_s {
    int hash;
    Obj *key;
    Obj *data;
    struct avltree_node node;
};

extern int pair_compare(const struct avltree_node *, const struct avltree_node *);
#endif
