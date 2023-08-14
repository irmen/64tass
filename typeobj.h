/*
    $Id: typeobj.h 2999 2023-08-12 21:46:25Z soci $

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
#include "stdbool.h"

struct iter_s;

extern struct Type *const TYPE_OBJ;

typedef enum Truth_types {
    TRUTH_BOOL, TRUTH_ALL, TRUTH_ANY
} Truth_types;

typedef enum Type_types {
    T_NONE, T_BOOL, T_BITS, T_INT, T_FLOAT, T_BYTES, T_STR, T_GAP, T_ADDRESS,
    T_SYMBOL, T_ANONSYMBOL, T_ERROR, T_OPER, T_COLONLIST, T_TUPLE, T_LIST,
    T_DICT, T_MACRO, T_SEGMENT, T_UNION, T_STRUCT, T_MFUNC, T_CODE, T_LBL,
    T_DEFAULT, T_REGISTER, T_FUNCTION, T_FUNCARGS, T_TYPE, T_LABEL,
    T_NAMESPACE, T_MEMBLOCKS, T_FOLD, T_SFUNC, T_ENC, T_ALIGNBLK
} Type_types;

typedef struct Type {
    Obj v;
    Type_types type;
    bool iterable;
    const char *name;
    struct Slot **slot;
    size_t length;
    Obj *(*convert)(struct oper_s *) MUST_CHECK;
    Obj *(*convert2)(struct oper_s *) MUST_CHECK;
    void (*destroy)(Obj *) FAST_CALL;
    void (*garbage)(Obj *, int) FAST_CALL;
    bool (*same)(const Obj *, const Obj *) FAST_CALL;
    Obj *(*truth)(Obj *, Truth_types, linepos_t) MUST_CHECK;
    Obj *(*hash)(Obj *, int *, linepos_t) MUST_CHECK;
    Obj *(*repr)(Obj *, linepos_t, size_t) MUST_CHECK;
    Obj *(*str)(Obj *, linepos_t, size_t) MUST_CHECK;
    Obj *(*calc1)(struct oper_s *) MUST_CHECK;
    Obj *(*calc2)(struct oper_s *) MUST_CHECK;
    Obj *(*rcalc2)(struct oper_s *) MUST_CHECK;
    Obj *(*slice)(struct oper_s *, argcount_t) MUST_CHECK;
    Obj *(*contains)(struct oper_s *) MUST_CHECK;
    struct Error *(*ival)(Obj *, ival_t *, unsigned int, linepos_t) MUST_CHECK;
    struct Error *(*uval)(Obj *, uval_t *, unsigned int, linepos_t) MUST_CHECK;
    struct Error *(*uval2)(Obj *, uval_t *, unsigned int, linepos_t) MUST_CHECK;
    uint32_t (*address)(const Obj *) FAST_CALL;
    struct Error *(*iaddress)(Obj *, ival_t *, unsigned int, linepos_t) MUST_CHECK;
    struct Error *(*uaddress)(Obj *, uval_t *, unsigned int, linepos_t) MUST_CHECK;
    Obj *(*sign)(Obj *, linepos_t) MUST_CHECK;
    Obj *(*function)(struct oper_s *) MUST_CHECK;
    Obj *(*len)(struct oper_s *) MUST_CHECK;
    Obj *(*size)(struct oper_s *) MUST_CHECK;
    void (*getiter)(struct iter_s *);
    void (*getriter)(struct iter_s *);
} Type;

enum { MAXIMUM_TYPE_LENGTH = 33 };

#define Type(a) OBJ_CAST(Type, a)

extern void init_type(void);
extern void typeobj_init(void);
extern void typeobj_names(void);
extern Type *new_type(Type *, Type_types, const char *, size_t);

#endif
