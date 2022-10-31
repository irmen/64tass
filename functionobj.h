/*
    $Id: functionobj.h 2884 2022-10-31 13:29:58Z soci $

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
#ifndef FUNCTIONOBJ_H
#define FUNCTIONOBJ_H
#include "obj.h"

extern struct Type *const FUNCTION_OBJ;

typedef enum Function_types {
    F_NONE, F_FLOOR, F_CEIL, F_ROUND, F_TRUNC, F_FRAC, F_SQRT, F_CBRT, F_LOG,
    F_LOG10, F_EXP, F_SIN, F_COS, F_TAN, F_ACOS, F_ASIN, F_ATAN, F_RAD, F_DEG,
    F_COSH, F_SINH, F_TANH, F_HYPOT, F_ATAN2, F_POW, F_SIGN, F_ABS, F_ALL,
    F_ANY, F_SIZE, F_LEN, F_RANGE, F_REPR, F_FORMAT, F_RANDOM, F_SORT,
    F_BINARY, F_BYTE, F_CHAR, F_RTA, F_ADDR, F_SINT, F_WORD, F_LINT, F_LONG,
    F_DINT, F_DWORD
} Function_types;

typedef struct Function {
    Obj v;
    const char name[7];
    const uint8_t name_len;
    int name_hash;
    const Function_types func;
} Function;

#define Function(a) OBJ_CAST(Function, a)

struct oper_s;

typedef MUST_CHECK Obj *(*apply_func_t)(struct oper_s *);

extern MUST_CHECK Obj *apply_function(struct oper_s *, apply_func_t);
extern MUST_CHECK Obj *apply_convert2(struct oper_s *);
extern MUST_CHECK Obj *apply_condition(struct oper_s *);
extern void functionobj_init(void);
extern void functionobj_names(void);
extern void functionobj_destroy(void);
extern void random_reseed(Obj *, linepos_t);

#endif
