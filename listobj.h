/*
    $Id: listobj.h 3086 2023-09-03 06:23:08Z soci $

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
#ifndef LISTOBJ_H
#define LISTOBJ_H
#include "obj.h"
#include "values.h"
#include "stdbool.h"

extern struct Type *const LIST_OBJ;
extern struct Type *const TUPLE_OBJ;
extern struct Type *const COLONLIST_OBJ;

typedef struct List {
    Obj v;
    size_t len;
    Obj **data;
    union {
        Obj *val[5];
        struct {
            size_t max;
            int hash;
        } s;
    } u;
} List;
typedef struct List Tuple;
typedef struct List Colonlist;

#define List(a) OBJ_CAST(List, a)
#define Tuple(a) OBJ_CAST(Tuple, a)
#define Colonlist(a) OBJ_CAST(Colonlist, a)

extern Obj *const null_tuple;
extern Obj *const null_list;

extern void listobj_init(void);
extern void listobj_names(void);
extern void listobj_destroy(void);

static inline MUST_CHECK List *new_list(void) {
    return List(val_alloc(LIST_OBJ));
}
static inline MUST_CHECK Colonlist *new_colonlist(void) {
    return Colonlist(val_alloc(COLONLIST_OBJ));
}

struct sliceparam_s {
    uval_t length;
    ival_t offset, end, step;
};

struct indexoffs_s {
    Obj *val;
    size_t len;
    size_t offs;
    linepos_t epoint;
};

extern MUST_CHECK Obj *indexoffs(struct indexoffs_s *);
extern MUST_CHECK Obj *sliceparams(struct sliceparam_s *, const struct indexoffs_s *);
extern MUST_CHECK Tuple *new_tuple(size_t);
extern Obj **list_create_elements(List *, size_t);
extern MUST_CHECK bool list_extend(List *);
extern void list_shrink(List *, size_t);
#endif
