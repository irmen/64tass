/*
    $Id: obj.h 3000 2023-08-13 05:26:27Z soci $

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
#define OBJ_CAST(b, a) ((b *)(1 ? (a) : (Obj *)(b *)(a)))

typedef struct Lbl {
    Obj v;
    linenum_t sline;
    uint8_t pass;
    size_t waitforp;
    const struct file_list_s *file_list;
    struct Namespace *parent;
} Lbl;

#define Lbl(a) OBJ_CAST(Lbl, a)

typedef struct Funcargs {
    Obj v;
    argcount_t len;
    struct values_s *val;
} Funcargs;

#define Funcargs(a) OBJ_CAST(Funcargs, a)

typedef struct Default {
    Obj v;
} Default;

#define Default(a) OBJ_CAST(Default, a)

typedef struct Alignblk {
    Obj v;
    address_t addr;
    address_t size;
    uint8_t pass;
} Alignblk;

#define Alignblk(a) OBJ_CAST(Alignblk, a)

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
extern struct Type *const ALIGNBLK_OBJ;
extern Obj *const default_value;

static inline Obj *ref_default(void) {
    default_value->refcount++; return default_value;
}
#endif
