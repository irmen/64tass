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
#include <stdio.h>
#include "inttypes.h"
#include "values.h"

struct oper_s;
typedef struct Error Error;
typedef struct Namespace Namespace;
typedef struct Int Int;

typedef struct Obj {
    obj_t obj;
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

typedef struct Macro {
    Obj v;
    size_t argc;
    struct macro_param_s *param;
    struct file_list_s *file_list;
    line_t line;
    int retval;
} Macro;

typedef struct Macro Segment;

typedef struct Struct {
    Obj v;
    size_t argc;
    struct macro_param_s *param;
    struct file_list_s *file_list;
    line_t line;
    size_t size; /* first part same as macro! */
    Namespace *names;
} Struct;

typedef struct Struct Union;

typedef struct Lbl_s {
    Obj v;
    line_t sline;
    size_t waitforp;
    const struct file_list_s *file_list;
    Namespace *parent;
} Lbl;

typedef struct Type {
    Obj v;
    obj_t type;
} Type;

typedef struct Oper {
    Obj v;
    const char *name;
    enum oper_e op;
    int prio;
} Oper;

typedef struct Funcargs {
    Obj v;
    size_t len;
    struct values_s *val;
} Funcargs;

typedef struct Iter {
    Obj v;
    void *iter;
    size_t val;
    Obj *data;
} Iter;

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

typedef struct None {
    Obj v;
    int *dummy;
} None;

typedef struct Gap {
    Obj v;
    int *dummy;
} Gap;

typedef struct Default {
    Obj v;
    int *dummy;
} Default;

enum type_e {
    T_NONE, T_BOOL, T_BITS, T_INT, T_FLOAT, T_BYTES, T_STR, T_GAP, T_ADDRESS,
    T_IDENT, T_ANONIDENT, T_ERROR, T_OPER, T_COLONLIST, T_TUPLE, T_LIST,
    T_DICT, T_MACRO, T_SEGMENT, T_UNION, T_STRUCT, T_MFUNC, T_CODE, T_LBL,
    T_DEFAULT, T_ITER, T_REGISTER, T_FUNCTION, T_ADDRLIST, T_FUNCARGS, T_TYPE,
    T_LABEL, T_NAMESPACE
};

enum truth_e {
    TRUTH_BOOL, TRUTH_ALL, TRUTH_ANY
};

struct obj_s {
    enum type_e type;
    const char *name;
    size_t length;
    Obj *(*create)(Obj *, linepos_t) MUST_CHECK;
    void (*destroy)(Obj *);
    void (*garbage)(Obj *, int);
    int (*same)(const Obj *, const Obj *);
    Obj *(*truth)(Obj *, enum truth_e, linepos_t) MUST_CHECK;
    Error *(*hash)(Obj *, int *, linepos_t) MUST_CHECK;
    Obj *(*repr)(Obj *, linepos_t) MUST_CHECK;
    Obj *(*calc1)(struct oper_s *) MUST_CHECK;
    Obj *(*calc2)(struct oper_s *) MUST_CHECK;
    Obj *(*rcalc2)(struct oper_s *) MUST_CHECK;
    Error *(*ival)(Obj *, ival_t *, int, linepos_t) MUST_CHECK;
    Error *(*uval)(Obj *, uval_t *, int, linepos_t) MUST_CHECK;
    Error *(*address)(Obj *, uval_t *, int, uint32_t *, linepos_t) MUST_CHECK;
    Obj *(*sign)(Obj *, linepos_t) MUST_CHECK;
    Obj *(*abs)(Obj *, linepos_t) MUST_CHECK;
    Obj *(*len)(Obj *, linepos_t) MUST_CHECK;
    Obj *(*size)(Obj *, linepos_t) MUST_CHECK;
    Iter *(*getiter)(Obj *) MUST_CHECK;
    Obj *(*next)(Iter *) MUST_CHECK;
};

static inline MUST_CHECK int obj_same(const Obj *v1, const Obj *v2) {
    return v1->obj->same(v1, v2);
}

static inline MUST_CHECK Error *obj_hash(Obj *v1, int *hs, linepos_t epoint) {
    return v1->obj->hash(v1, hs, epoint);
}

static inline Obj *val_reference(Obj *v1) {
    v1->refcount++; return v1;
}

static inline Obj *obj_next(Iter *v1) {
    return v1->v.obj->next(v1);
}

extern void obj_init(struct obj_s *, enum type_e, const char *, size_t);
extern MUST_CHECK Obj *obj_oper_error(struct oper_s *);
extern void objects_init(void);
extern void objects_destroy(void);
extern MUST_CHECK Iter *invalid_getiter(Obj *);

extern obj_t LBL_OBJ;
extern obj_t MACRO_OBJ;
extern obj_t SEGMENT_OBJ;
extern obj_t MFUNC_OBJ;
extern obj_t STRUCT_OBJ;
extern obj_t UNION_OBJ;
extern obj_t NONE_OBJ;
extern obj_t ERROR_OBJ;
extern obj_t GAP_OBJ;
extern obj_t IDENT_OBJ;
extern obj_t ANONIDENT_OBJ;
extern obj_t OPER_OBJ;
extern obj_t DEFAULT_OBJ;
extern obj_t ITER_OBJ;
extern obj_t FUNCARGS_OBJ;
extern obj_t TYPE_OBJ;
extern None *none_value;
extern Gap *gap_value;
extern Default *default_value;

static inline None *ref_none(void) {
    none_value->v.refcount++; return none_value;
}

static inline Gap *ref_gap(void) {
    gap_value->v.refcount++; return gap_value;
}

static inline Default *ref_default(void) {
    default_value->v.refcount++; return default_value;
}

extern int referenceit;
#endif
