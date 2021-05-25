/*
    $Id: namespaceobj.h 2651 2021-05-09 19:33:48Z soci $

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
#ifndef NAMESPACEOBJ_H
#define NAMESPACEOBJ_H
#include "obj.h"

extern struct Type *const NAMESPACE_OBJ;

struct Label;

typedef struct Namespace {
    Obj v;
    size_t len, mask;
    struct Label **data;
    const struct file_list_s *file_list;
    struct linepos_s epoint;
    uint32_t backr, forwr;
} Namespace;

#define Namespace(a) OBJ_CAST(Namespace, a)

extern void namespaceobj_init(void);
extern void namespaceobj_names(void);

static inline Namespace *ref_namespace(Namespace *v1) {
    v1->v.refcount++; return v1;
}

extern MALLOC Namespace *new_namespace(const struct file_list_s *, linepos_t);
extern MUST_CHECK Obj *namespace_member(struct oper_s *, Namespace *);
extern Namespace *get_namespace(const Obj *);

#endif
