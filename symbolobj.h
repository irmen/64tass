/*
    $Id: symbolobj.h 2651 2021-05-09 19:33:48Z soci $

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
#ifndef SYMBOLOBJ_H
#define SYMBOLOBJ_H
#include "obj.h"
#include "str.h"
#include "stdbool.h"

extern struct Type *const SYMBOL_OBJ;

struct file_list_s;

typedef struct Symbol {
    Obj v;
    str_t name;
    str_t cfname;
    int hash;
    const struct file_list_s *file_list;
    struct linepos_s epoint;
} Symbol;

#define Symbol(a) OBJ_CAST(Symbol, a)

extern void symbolobj_init(void);

extern Obj *new_symbol(const str_t *name, linepos_t);

extern bool symbol_cfsame(Symbol *, Symbol *);
static inline Symbol *ref_symbol(Symbol *v1) {
    v1->v.refcount++; return v1;
}
#endif
