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

struct oper_s;

struct mfunc_param_s {
    str_t name;
    str_t cfname;
    value_t init;
    struct linepos_s epoint;
};

typedef struct {
    struct label_s *label;
    size_t argc;
    struct mfunc_param_s *param; 
} mfunc_t;

struct macro_param_s {
    str_t cfname;
    str_t init;
}; 

typedef struct {
    size_t size;
    size_t argc;
    struct macro_param_s *param; 
    struct label_s *label;
} macro_t;

enum type_e {
    T_NONE, T_BOOL, T_BITS, T_INT, T_FLOAT, T_BYTES, T_STR, T_GAP, T_ADDRESS,
    T_IDENT, T_ANONIDENT, T_ERROR, T_OPER, T_COLONLIST, T_TUPLE, T_LIST,
    T_DICT, T_MACRO, T_SEGMENT, T_UNION, T_STRUCT, T_MFUNC, T_CODE, T_LBL,
    T_DEFAULT, T_ITER, T_REGISTER, T_FUNCTION, T_ADDRLIST, T_FUNCARGS, T_TYPE
};

enum truth_e {
    TRUTH_BOOL, TRUTH_ALL, TRUTH_ANY
};

struct obj_s {
    enum type_e type;
    const char *name;
    value_t (*create)(value_t, linepos_t) MUST_CHECK;
    void (*destroy)(value_t);
    int (*same)(const value_t, const value_t);
    value_t (*truth)(const value_t, enum truth_e, linepos_t) MUST_CHECK;
    value_t (*hash)(const value_t, int *, linepos_t) MUST_CHECK;
    value_t (*repr)(const value_t, linepos_t) MUST_CHECK;
    value_t (*calc1)(struct oper_s *) MUST_CHECK;
    value_t (*calc2)(struct oper_s *) MUST_CHECK;
    value_t (*rcalc2)(struct oper_s *) MUST_CHECK;
    value_t (*ival)(const value_t, ival_t *, int, linepos_t) MUST_CHECK;
    value_t (*uval)(const value_t, uval_t *, int, linepos_t) MUST_CHECK;
    value_t (*sign)(const value_t, linepos_t) MUST_CHECK;
    value_t (*abs)(const value_t, linepos_t) MUST_CHECK;
    value_t (*len)(const value_t, linepos_t) MUST_CHECK;
    value_t (*size)(const value_t, linepos_t) MUST_CHECK;
    value_t (*getiter)(value_t) MUST_CHECK;
    value_t (*next)(value_t) MUST_CHECK;
};

extern void obj_init(struct obj_s *, enum type_e, const char *);
extern MUST_CHECK value_t obj_oper_error(struct oper_s *);
extern void objects_init(void);
extern MUST_CHECK value_t invalid_getiter(value_t);

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

extern int referenceit;
#endif
