/*
    $Id: obj.h 2547 2021-03-19 23:40:46Z soci $

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
#ifndef OBJ_H
#define OBJ_H
#include "attributes.h"
#include "inttypes.h"

struct oper_s;
struct Type;

typedef struct Obj {
    const struct Type *obj;
    size_t refcount;
} Obj;

#define Obj(a) ((0 ? &(a)->v : (Obj *)(a)))

typedef struct Lbl {
    Obj v;
    linenum_t sline;
    size_t waitforp;
    const struct file_list_s *file_list;
    struct Namespace *parent;
} Lbl;

#define Lbl(a) ((Lbl *)(1 ? (a) : (Obj *)(Lbl *)(a)))

typedef struct Funcargs {
    Obj v;
    argcount_t len;
    struct values_s *val;
} Funcargs;

#define Funcargs(a) ((Funcargs *)(1 ? (a) : (Obj *)(Funcargs *)(a)))

typedef struct Default {
    Obj v;
} Default;

#define Default(a) ((Default *)(1 ? (a) : (Obj *)(Default *)(a)))

static inline Obj *val_reference(Obj *v1) {
    v1->refcount++; return v1;
}

extern MUST_CHECK Obj *obj_oper_error(struct oper_s *);
extern MUST_CHECK Obj *obj_oper_compare(struct oper_s *, int);
extern void obj_init(struct Type *);
extern void objects_init(void);
extern void objects_destroy(void);

extern struct Type *const LBL_OBJ;
extern struct Type *const DEFAULT_OBJ;
extern struct Type *const FUNCARGS_OBJ;
extern Obj *const default_value;

static inline Obj *ref_default(void) {
    default_value->refcount++; return default_value;
}
#endif
