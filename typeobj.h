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
#ifndef TYPEOBJ_H
#define TYPEOBJ_H
#include "obj.h"

extern struct Type *TYPE_OBJ;

enum truth_e {
    TRUTH_BOOL, TRUTH_ALL, TRUTH_ANY
};

enum type_e {
    T_NONE, T_BOOL, T_BITS, T_INT, T_FLOAT, T_BYTES, T_STR, T_GAP, T_ADDRESS,
    T_IDENT, T_ANONIDENT, T_ERROR, T_OPER, T_COLONLIST, T_TUPLE, T_LIST,
    T_DICT, T_MACRO, T_SEGMENT, T_UNION, T_STRUCT, T_MFUNC, T_CODE, T_LBL,
    T_DEFAULT, T_ITER, T_REGISTER, T_FUNCTION, T_ADDRLIST, T_FUNCARGS, T_TYPE,
    T_LABEL, T_NAMESPACE
};

struct Error;

typedef struct Type {
    Obj v;
    enum type_e type;
    const char *name;
    size_t length;
    Obj *(*create)(Obj *, linepos_t) MUST_CHECK;
    void (*destroy)(Obj *);
    void (*garbage)(Obj *, int);
    bool (*same)(const Obj *, const Obj *);
    Obj *(*truth)(Obj *, enum truth_e, linepos_t) MUST_CHECK;
    struct Error *(*hash)(Obj *, int *, linepos_t) MUST_CHECK;
    Obj *(*repr)(Obj *, linepos_t, size_t) MUST_CHECK;
    Obj *(*calc1)(struct oper_s *) MUST_CHECK;
    Obj *(*calc2)(struct oper_s *) MUST_CHECK;
    Obj *(*rcalc2)(struct oper_s *) MUST_CHECK;
    Obj *(*slice)(Obj *, struct oper_s *, size_t) MUST_CHECK;
    struct Error *(*ival)(Obj *, ival_t *, unsigned int, linepos_t) MUST_CHECK;
    struct Error *(*uval)(Obj *, uval_t *, unsigned int, linepos_t) MUST_CHECK;
    struct Error *(*address)(Obj *, uval_t *, int, uint32_t *, linepos_t) MUST_CHECK;
    Obj *(*sign)(Obj *, linepos_t) MUST_CHECK;
    Obj *(*absolute)(Obj *, linepos_t) MUST_CHECK;
    Obj *(*len)(Obj *, linepos_t) MUST_CHECK;
    Obj *(*size)(Obj *, linepos_t) MUST_CHECK;
    Iter *(*getiter)(Obj *) MUST_CHECK;
    Obj *(*next)(Iter *) MUST_CHECK;
} Type;

extern void typeobj_init(void);
extern void typeobj_names(void);

static inline void new_type(Type *obj, enum type_e type, const char *name, size_t length) {
    obj->v.obj = TYPE_OBJ;
    obj->v.refcount = 1;
    obj->type = type;
    obj->length = length;
    obj->name = name;
}

#endif
