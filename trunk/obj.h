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
#ifndef _OBJ_H
#define _OBJ_H
#include "inttypes.h"

struct oper_s;
struct Namespace;
struct Type;

typedef struct Obj {
    struct Type *obj;
    size_t refcount;
} Obj;

struct mfunc_param_s {
    str_t name;
    str_t cfname;
    Obj *init;
    struct linepos_s epoint;
};

typedef struct Mfunc {
    Obj v;
    size_t argc;
    struct mfunc_param_s *param; 
    struct file_list_s *file_list;
    line_t line;
} Mfunc;

struct macro_param_s {
    str_t cfname;
    str_t init;
}; 

typedef struct Struct {
    Obj v;
    size_t argc;
    struct macro_param_s *param;
    struct file_list_s *file_list;
    line_t line;
    size_t size; /* first part same as macro! */
    struct Namespace *names;
} Struct;

typedef struct Struct Union;

typedef struct Lbl_s {
    Obj v;
    line_t sline;
    size_t waitforp;
    const struct file_list_s *file_list;
    struct Namespace *parent;
} Lbl;

typedef struct Iter {
    Obj v;
    void *iter;
    size_t val;
    Obj *data;
} Iter;

typedef struct Funcargs {
    Obj v;
    size_t len;
    struct values_s *val;
} Funcargs;

typedef struct Ident {
    Obj v;
    str_t name;
    struct linepos_s epoint;
} Ident;

typedef struct Anonident {
    Obj v;
    int32_t count;
    struct linepos_s epoint;
} Anonident;

typedef struct Default {
    Obj v;
    int *dummy;
} Default;

static inline Obj *val_reference(Obj *v1) {
    v1->refcount++; return v1;
}

extern MUST_CHECK Obj *obj_oper_error(struct oper_s *);
extern void obj_init(struct Type *);
extern void objects_init(void);
extern void objects_destroy(void);
extern MUST_CHECK Iter *invalid_getiter(Obj *);

extern struct Type *LBL_OBJ;
extern struct Type *MFUNC_OBJ;
extern struct Type *STRUCT_OBJ;
extern struct Type *UNION_OBJ;
extern struct Type *IDENT_OBJ;
extern struct Type *ANONIDENT_OBJ;
extern struct Type *DEFAULT_OBJ;
extern struct Type *ITER_OBJ;
extern struct Type *FUNCARGS_OBJ;
extern Default *default_value;

static inline Default *ref_default(void) {
    default_value->v.refcount++; return default_value;
}

extern int referenceit;
#endif
