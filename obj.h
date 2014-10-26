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
struct value_s;
struct oper_s;
struct values_s;

#define obj_print(v, f) val_print(v, f)
#define obj_destroy(v) do { struct value_s *_v_ = (v); _v_->obj->destroy(_v_); } while (0)
#define obj_same(v, v2) v->obj->same(v, v2)
#define obj_hash(v, v2, epoint) v->obj->hash(v, v2, epoint)

struct pair_s {
    int hash;
    struct value_s *key;
    struct value_s *data;
    struct avltree_node node;
};

typedef struct {
    size_t len;
    struct avltree members;
    struct value_s *def;
} dict_t;

struct mfunc_param_s {
    str_t name;
    str_t cfname;
    struct value_s *init;
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
    T_DEFAULT, T_ITER, T_REGISTER, T_FUNCTION, T_ADDRLIST
};

enum truth_e {
    TRUTH_BOOL, TRUTH_ALL, TRUTH_ANY
};

typedef const struct obj_s* obj_t;
struct obj_s {
    enum type_e type;
    const char *name;
    void (*destroy)(struct value_s *);
    void (*copy)(const struct value_s *, struct value_s *);
    void (*copy_temp)(const struct value_s *, struct value_s *);
    int (*same)(const struct value_s *, const struct value_s *);
    int (*truth)(const struct value_s *, struct value_s *, enum truth_e, linepos_t);
    int (*hash)(const struct value_s *, struct value_s *, linepos_t);
    void (*repr)(const struct value_s *, struct value_s *, linepos_t);
    void (*str)(const struct value_s *, struct value_s *, linepos_t);
    void (*calc1)(struct oper_s *);
    struct value_s *(*calc2)(struct oper_s *) MUST_CHECK;
    struct value_s *(*rcalc2)(struct oper_s *) MUST_CHECK;
    struct value_s *(*repeat)(struct oper_s *, uval_t) MUST_CHECK;
    struct value_s *(*iindex)(struct oper_s *) MUST_CHECK;
    int (*ival)(const struct value_s *, struct value_s *, ival_t *, int, linepos_t) MUST_CHECK;
    int (*uval)(const struct value_s *, struct value_s *, uval_t *, int, linepos_t) MUST_CHECK;
    int (*real)(const struct value_s *, struct value_s *, double *, linepos_t) MUST_CHECK;
    void (*sign)(const struct value_s *, struct value_s *, linepos_t);
    void (*abs)(const struct value_s *, struct value_s *, linepos_t);
    void (*integer)(const struct value_s *, struct value_s *, linepos_t);
    void (*len)(const struct value_s *, struct value_s *, linepos_t);
    void (*size)(const struct value_s *, struct value_s *, linepos_t);
    void (*getiter)(struct value_s *, struct value_s *);
    struct value_s *(*next)(struct value_s *, struct value_s *) MUST_CHECK;
};

extern void obj_init(struct obj_s *, enum type_e, const char *);
extern struct value_s *obj_oper_error(struct oper_s *);
extern void objects_init(void);
extern void invalid_getiter(struct value_s *, struct value_s *);

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
extern obj_t DICT_OBJ;
extern obj_t ITER_OBJ;

extern int referenceit;
#endif
