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
#include <string.h>
#include "values.h"
#include "variables.h"
#include "misc.h"
#include "section.h"
#include "64tass.h"
#include "eval.h"

#include "boolobj.h"
#include "floatobj.h"
#include "macro.h"

int referenceit = 1;

static struct obj_s macro_obj;
static struct obj_s segment_obj;
static struct obj_s lbl_obj;
static struct obj_s mfunc_obj;
static struct obj_s struct_obj;
static struct obj_s union_obj;
static struct obj_s none_obj;
static struct obj_s error_obj;
static struct obj_s gap_obj;
static struct obj_s ident_obj;
static struct obj_s anonident_obj;
static struct obj_s oper_obj;
static struct obj_s default_obj;
static struct obj_s iter_obj;
static struct obj_s funcargs_obj;
static struct obj_s type_obj;

obj_t MACRO_OBJ = &macro_obj;
obj_t SEGMENT_OBJ = &segment_obj;
obj_t LBL_OBJ = &lbl_obj;
obj_t MFUNC_OBJ = &mfunc_obj;
obj_t STRUCT_OBJ = &struct_obj;
obj_t UNION_OBJ = &union_obj;
obj_t NONE_OBJ = &none_obj;
obj_t ERROR_OBJ = &error_obj;
obj_t GAP_OBJ = &gap_obj;
obj_t IDENT_OBJ = &ident_obj;
obj_t ANONIDENT_OBJ = &anonident_obj;
obj_t OPER_OBJ = &oper_obj;
obj_t DEFAULT_OBJ = &default_obj;
obj_t ITER_OBJ = &iter_obj;
obj_t FUNCARGS_OBJ = &funcargs_obj;
obj_t TYPE_OBJ = &type_obj;

MUST_CHECK value_t obj_oper_error(oper_t op) {
    value_t v1, v2, v;
    switch (op->op->u.oper.op) {
    case O_EQ: return val_reference(false_value);
    case O_NE: return val_reference(true_value);
    default: break;
    }
    v1 = op->v1; v2 = op->v2;
    v = new_error_obj(ERROR__INVALID_OPER, op->epoint3);
    v->u.error.u.invoper.op = op->op;
    v->u.error.u.invoper.v1 = v1 ? (v1->refcount ? val_reference(v1) : v1) : NULL;
    v->u.error.u.invoper.v2 = v2 ? (v2->refcount ? val_reference(v2) : v2) : NULL;
    return v;
}

static void invalid_destroy(value_t UNUSED(v1)) {
    return;
}

static MUST_CHECK value_t invalid_create(const value_t v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR: return val_reference(v1);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return val_reference(none_value);
}

static int invalid_same(const value_t v1, const value_t v2) {
    return v1->obj == v2->obj;
}

static MUST_CHECK value_t generic_invalid(value_t v1, linepos_t epoint, enum errors_e num) {
    value_t v;
    if (v1->obj == ERROR_OBJ) {
        return val_reference(v1);
    }
    v = new_error_obj(num, epoint);
    v->u.error.u.objname = v1->obj->name;
    return v;
}

static MUST_CHECK value_t invalid_truth(const value_t v1, enum truth_e UNUSED(type), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR_____CANT_BOOL);
}

static MUST_CHECK value_t invalid_hash(const value_t v1, int *UNUSED(hash), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR__NOT_HASHABLE);
}

static MUST_CHECK value_t invalid_repr(const value_t v1, linepos_t epoint) {
    value_t v;
    uint8_t *s;
    const char *name;
    if (!epoint) return NULL;
    if (v1->obj == ERROR_OBJ) {
        return val_reference(v1);
    }
    name = v1->obj->name;
    v = val_alloc(STR_OBJ);
    v->u.str.len = strlen(name) + 2;
    v->u.str.chars = v->u.str.len;
    s = str_create_elements(v, v->u.str.len);
    *s = '<';
    memcpy(s + 1, name, v->u.str.len);
    s[v->u.str.len - 1] = '>';
    v->u.str.data = s;
    return v;
}

static MUST_CHECK value_t gap_create(const value_t v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_GAP: return val_reference(v1);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return val_reference(none_value);
}

static MUST_CHECK value_t gap_hash(const value_t UNUSED(v1), int *hs, linepos_t UNUSED(epoint)) {
    *hs = 0; /* whatever, there's only one */
    return NULL;
}

static MUST_CHECK value_t gap_repr(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    uint8_t *s;
    value_t v = val_alloc(STR_OBJ);
    v->u.str.len = 1;
    v->u.str.chars = 1;
    s = str_create_elements(v, 1);
    *s = '?';
    v->u.str.data = s;
    return v;
}

static MUST_CHECK value_t gap_calc1(oper_t op) {
    switch (op->op->u.oper.op) {
    case O_BANK: 
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
    case O_INV:
    case O_NEG:
    case O_POS:
    case O_STRING:
        return val_reference(gap_value);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t gap_calc2(oper_t op) {
    value_t v2 = op->v2;
    switch (v2->obj->type) {
    case T_GAP:
        switch (op->op->u.oper.op) {
        case O_CMP: return val_reference(int_value[0]);
        case O_GE: 
        case O_LE:
        case O_EQ: return val_reference(true_value);
        case O_NE:
        case O_LT:
        case O_GT: return val_reference(false_value);
        case O_ADD:
        case O_SUB:
        case O_MUL:
        case O_DIV:
        case O_MOD:
        case O_EXP:
        case O_AND:
        case O_OR:
        case O_XOR:
        case O_LSHIFT:
        case O_RSHIFT: return val_reference(gap_value);
        default: break;
        }
        break;
    case T_STR:
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
    case T_BYTES:
    case T_REGISTER:
        switch (op->op->u.oper.op) {
        case O_EQ: return val_reference(false_value);
        case O_NE: return val_reference(true_value);
        default: break;
        }
        break;
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
    case T_DICT:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t gap_rcalc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    switch (v1->obj->type) {
    case T_GAP: return gap_calc2(op);
    case T_STR:
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
    case T_BYTES:
    case T_REGISTER:
        switch (op->op->u.oper.op) {
        case O_EQ: return val_reference(false_value);
        case O_NE: return val_reference(true_value);
        default: break;
        }
        break;
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
        return v2->obj->calc2(op);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t error_calc1(oper_t op) {
    return val_reference(op->v1);
}

static MUST_CHECK value_t error_calc2(oper_t op) {
    return val_reference(op->v1);
}

static MUST_CHECK value_t error_rcalc2(oper_t op) {
    return val_reference(op->v2);
}

static MUST_CHECK value_t invalid_calc1(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return error_calc1(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t invalid_calc2(oper_t op) {
    if (op->v2->obj == ERROR_OBJ) {
        return error_rcalc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t invalid_rcalc2(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return error_calc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t invalid_ival(const value_t v1, ival_t *UNUSED(iv), int UNUSED(bits), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_INT);
}

static MUST_CHECK value_t invalid_uval(const value_t v1, uval_t *UNUSED(uv), int UNUSED(bits), linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_INT);
}

static MUST_CHECK value_t invalid_sign(const value_t v1, linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR_____CANT_SIGN);
}

static MUST_CHECK value_t invalid_abs(const value_t v1, linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_ABS);
}

static MUST_CHECK value_t invalid_len(const value_t v1, linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR______CANT_LEN);
}

static MUST_CHECK value_t invalid_size(const value_t v1, linepos_t epoint) {
    return generic_invalid(v1, epoint, ERROR_____CANT_SIZE);
}

MUST_CHECK value_t invalid_getiter(value_t v1) {
    value_t v = val_alloc(ITER_OBJ);
    v->u.iter.data = val_reference(v1);
    v->u.iter.iter = NULL;
    v->u.iter.val = 1;
    return v;
}

static MUST_CHECK value_t invalid_next(value_t v1) {
    if (v1->u.iter.val) {
        v1->u.iter.val = 0;
        return val_reference(v1->u.iter.data);
    }
    return NULL;
}

static void iter_destroy(value_t v1) {
    if (v1->u.iter.iter != &v1->u.iter.val) free(v1->u.iter.iter);
    val_destroy((value_t)v1->u.iter.data);
}

static MUST_CHECK value_t iter_next(value_t v1) {
    if (!v1->u.iter.iter) return invalid_next(v1);
    return v1->u.iter.data->obj->next(v1);
}

static void macro_destroy(value_t v1) {
    while (v1->u.macro.argc) {
        --v1->u.macro.argc;
        free((char *)v1->u.macro.param[v1->u.macro.argc].cfname.data);
        free((char *)v1->u.macro.param[v1->u.macro.argc].init.data);
    }
    free(v1->u.macro.param);
    if (v1->u.macro.label->parent == NULL) label_destroy(v1->u.macro.label);
}

static int macro_same(const value_t v1, const value_t v2) {
    size_t i;
    if (v1->obj != v2->obj || v1->u.macro.size != v2->u.macro.size || v1->u.macro.label != v2->u.macro.label) return 0;
    for (i = 0; i < v1->u.macro.argc; i++) {
        if (str_cmp(&v1->u.macro.param[i].cfname, &v2->u.macro.param[i].cfname)) return 0;
        if (str_cmp(&v1->u.macro.param[i].init, &v2->u.macro.param[i].init)) return 0;
    }
    return 1;
}

static void mfunc_destroy(value_t v1) {
    size_t i = v1->u.mfunc.argc;
    while (i--) {
        free((char *)v1->u.mfunc.param[i].name.data);
        if (v1->u.mfunc.param[i].name.data != v1->u.mfunc.param[i].cfname.data) free((char *)v1->u.mfunc.param[i].cfname.data);
        if (v1->u.mfunc.param[i].init) val_destroy(v1->u.mfunc.param[i].init);
    }
    free(v1->u.mfunc.param);
}

static int mfunc_same(const value_t v1, const value_t v2) {
    size_t i;
    if (v2->obj != MFUNC_OBJ || v1->u.mfunc.label != v2->u.mfunc.label) return 0;
    for (i = 0; i < v1->u.mfunc.argc; i++) {
        if (str_cmp(&v1->u.mfunc.param[i].name, &v2->u.mfunc.param[i].name)) return 0;
        if ((v1->u.mfunc.param[i].name.data != v1->u.mfunc.param[i].cfname.data || v2->u.mfunc.param[i].name.data != v2->u.mfunc.param[i].cfname.data) && str_cmp(&v1->u.mfunc.param[i].cfname, &v2->u.mfunc.param[i].cfname)) return 0;
        if (v1->u.mfunc.param[i].init != v2->u.mfunc.param[i].init && (!v1->u.mfunc.param[i].init || !v2->u.mfunc.param[i].init || !obj_same(v1->u.mfunc.param[i].init, v2->u.mfunc.param[i].init))) return 0;
        if (v1->u.mfunc.param[i].epoint.pos != v2->u.mfunc.param[i].epoint.pos) return 0;
    }
    return 1;
}

static MUST_CHECK value_t mfunc_calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_FUNCARGS: 
        switch (op->op->u.oper.op) {
        case O_FUNC:
        {
            struct value_s *val, *v1 = op->v1, *v2 = op->v2;
            size_t i, max = 0, args = v2->u.funcargs.len;
            for (i = 0; i < args; i++) {
                if (v2->u.funcargs.val[i].val->obj == NONE_OBJ || v2->u.funcargs.val[i].val->obj == ERROR_OBJ) {
                    return val_reference(v2->u.funcargs.val[i].val);
                }
            }
            for (; i < v1->u.mfunc.argc; i++) {
                if (!v1->u.mfunc.param[i].init) {
                    max = i + 1;
                }
            }
            if (max) err_msg_argnum(args, max, v1->u.mfunc.argc, op->epoint2);
            eval_enter();
            val = mfunc2_recurse(v1, v2->u.funcargs.val, args, op->epoint);
            eval_leave();
            return val ? val : val_reference(null_tuple);
        }
        default: break;
        }
        break;
    default: break;
    }
    return obj_oper_error(op);
}

static void error_destroy(value_t v1) {
    switch (v1->u.error.num) {
    case ERROR__INVALID_OPER:
        if (v1->u.error.u.invoper.v1) val_destroy(v1->u.error.u.invoper.v1);
        if (v1->u.error.u.invoper.v2) val_destroy(v1->u.error.u.invoper.v2);
        return;
    case ERROR___NO_REGISTER: 
        val_destroy(v1->u.error.u.reg);
        return;
    default: return;
    }
}

static MUST_CHECK value_t oper_repr(const value_t v1, linepos_t epoint) {
    const char *txt;
    size_t len;
    uint8_t *s;
    value_t v;
    if (!epoint) return NULL;
    v = val_alloc(STR_OBJ);
    txt = v1->u.oper.name;
    len = strlen(txt);
    s = str_create_elements(v, len + 8);
    memcpy(s, "<oper ", 6);
    memcpy(s + 6, txt, len);
    len += 6;
    s[len++] = '\'';
    s[len++] = '>';
    v->u.str.data = s;
    v->u.str.len = len;
    v->u.str.chars = len;
    return v;
}

static MUST_CHECK value_t ident_calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return op->v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t ident_rcalc2(oper_t op) {
    if (op->op == &o_MEMBER) {
        return op->v1->obj->calc2(op);
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t none_truth(const value_t UNUSED(v1), enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_hash(const value_t UNUSED(v1), int *UNUSED(v), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_repr(const value_t UNUSED(v1), linepos_t epoint) {
    if (!epoint) return NULL;
    return val_reference(none_value);
}

static MUST_CHECK value_t none_calc1(oper_t UNUSED(op)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_calc2(oper_t op) {
    if (op->v2->obj == ERROR_OBJ) {
        return error_rcalc2(op);
    }
    return val_reference(none_value);
}

static MUST_CHECK value_t none_rcalc2(oper_t op) {
    if (op->v1->obj == ERROR_OBJ) {
        return error_calc2(op);
    }
    return val_reference(none_value);
}

static MUST_CHECK value_t none_ival(const value_t UNUSED(v1), ival_t *UNUSED(iv), int UNUSED(bits), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_uval(const value_t UNUSED(v1), uval_t *UNUSED(uv), int UNUSED(bits), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_sign(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_abs(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_len(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static MUST_CHECK value_t none_size(const value_t UNUSED(v1), linepos_t UNUSED(epoint)) {
    return val_reference(none_value);
}

static int lbl_same(const value_t v1, const value_t v2) {
    return v2->obj == LBL_OBJ && v1->u.lbl.sline == v2->u.lbl.sline && v1->u.lbl.waitforp == v2->u.lbl.waitforp && v1->u.lbl.file_list == v2->u.lbl.file_list && v1->u.lbl.parent == v2->u.lbl.parent;
}

static int funcargs_same(const value_t v1, const value_t v2) {
    return v1->u.funcargs.val == v2->u.funcargs.val && v1->u.funcargs.len == v2->u.funcargs.len;
}

static MUST_CHECK value_t type_create(const value_t v1, linepos_t UNUSED(epoint)) {
    value_t v;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_TYPE: return val_reference(v1);
    default: break;
    }
    v = val_alloc(TYPE_OBJ);
    v->u.type = v1->obj;
    return v;
}

static int type_same(const value_t v1, const value_t v2) {
    return v1->u.type == v2->u.type;
}

static MUST_CHECK value_t type_hash(const value_t v1, int *hs, linepos_t UNUSED(epoint)) {
    *hs = v1->u.type->type;
    return NULL;
}

static MUST_CHECK value_t type_repr(const value_t v1, linepos_t epoint) {
    value_t v;
    uint8_t *s;
    const char *name;
    size_t ln;
    if (!epoint) return NULL;
    name = v1->u.type->name;
    ln = strlen(name);
    v = val_alloc(STR_OBJ);
    v->u.str.len = ln + 9;
    v->u.str.chars = v->u.str.len;
    s = str_create_elements(v, v->u.str.len);
    memcpy(s, "<type '", 7);
    memcpy(s + 7, name, ln);
    memcpy(s + 7 + ln , "'>", 2);
    v->u.str.data = s;
    return v;
}

static inline int tcmp(const value_t vv1, const value_t vv2) {
    enum type_e v1 = vv1->u.type->type;
    enum type_e v2 = vv2->u.type->type;
    if (v1 < v2) return -1;
    return v1 > v2;
}

static MUST_CHECK value_t type_calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    size_t args;

    switch (v2->obj->type) {
    case T_TYPE:
        {
            int val;
            switch (op->op->u.oper.op) {
            case O_CMP:
                val = tcmp(v1, v2);
                if (val < 0) return int_from_int(-1);
                return val_reference(int_value[val > 0]);
            case O_EQ: return truth_reference(tcmp(v1, v2) == 0);
            case O_NE: return truth_reference(tcmp(v1, v2) != 0);
            case O_LT: return truth_reference(tcmp(v1, v2) < 0);
            case O_LE: return truth_reference(tcmp(v1, v2) <= 0);
            case O_GT: return truth_reference(tcmp(v1, v2) > 0);
            case O_GE: return truth_reference(tcmp(v1, v2) >= 0);
            default: break;
            }
        }
    case T_FUNCARGS:
        if (op->op == &o_FUNC) {
            args = v2->u.funcargs.len;
            if (args != 1) {
                err_msg_argnum(args, 1, 1, op->epoint2);
                return val_reference(none_value);
            }
            v2 = v2->u.funcargs.val[0].val;
            switch (v2->obj->type) {
            case T_LIST:
            case T_TUPLE: 
                if (v1->u.type != LIST_OBJ && v1->u.type != TUPLE_OBJ && v1->u.type != TYPE_OBJ) {
                    value_t *vals;
                    size_t i;
                    int error;
                    value_t v = val_alloc(v2->obj);
                    v->u.list.data = vals = list_create_elements(v, v2->u.list.len);
                    error = 1;
                    for (i = 0;i < v2->u.list.len; i++) {
                        value_t val = v1->u.type->create(v2->u.list.data[i], op->epoint2);
                        if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                        vals[i] = val;
                    }
                    v->u.list.len = i;
                    return v;
                }
                /* fall through */
            default: return v1->u.type->create(v2, op->epoint2);
            }
        }
        break;
    case T_NONE:
    case T_ERROR:
    case T_TUPLE:
    case T_LIST:
    case T_DICT:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t struct_size(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_size(v1->u.macro.size);
}

static MUST_CHECK value_t struct_calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    if (op->op == &o_MEMBER) {
        struct label_s *l, *l2;
        switch (v2->obj->type) {
        case T_IDENT:
            l2 = v1->u.macro.label;
            l = find_label2(&v2->u.ident.name, l2);
            if (l) {
                touch_label(l);
                return val_reference(l->value);
            }
            if (!referenceit) {
                return val_reference(none_value);
            }
            v = new_error_obj(ERROR___NOT_DEFINED, &v2->u.ident.epoint);
            v->u.error.u.notdef.label = l2;
            v->u.error.u.notdef.ident = v2->u.ident.name;
            v->u.error.u.notdef.down = 0;
            return v;
        case T_ANONIDENT:
            {
                ssize_t count;
                l2 = v1->u.macro.label;
                l = find_anonlabel2(v2->u.anonident.count, l2);
                if (l) {
                    touch_label(l);
                    return val_reference(l->value);
                }
                if (!referenceit) {
                    return val_reference(none_value);
                }
                count = v2->u.anonident.count;
                v = new_error_obj(ERROR___NOT_DEFINED, &v2->u.anonident.epoint);
                v->u.error.u.notdef.label = l2;
                v->u.error.u.notdef.ident.len = count + (count >= 0);
                v->u.error.u.notdef.ident.data = NULL;
                v->u.error.u.notdef.down = 0;
                return v;
            }
        case T_TUPLE:
        case T_LIST: return v2->obj->rcalc2(op);
        default: return v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

void obj_init(struct obj_s *obj, enum type_e type, const char *name) {
    obj->type = type;
    obj->name = name;
    obj->create = invalid_create;
    obj->destroy = invalid_destroy;
    obj->same = invalid_same;
    obj->truth = invalid_truth;
    obj->hash = invalid_hash;
    obj->repr = invalid_repr;
    obj->calc1 = invalid_calc1;
    obj->calc2 = invalid_calc2;
    obj->rcalc2 = invalid_rcalc2;
    obj->ival = invalid_ival;
    obj->uval = invalid_uval;
    obj->sign = invalid_sign;
    obj->abs = invalid_abs;
    obj->len = invalid_len;
    obj->size = invalid_size;
    obj->getiter = invalid_getiter;
    obj->next = invalid_next;
}

void objects_init(void) {
    boolobj_init();
    floatobj_init();
    addressobj_init();
    codeobj_init();
    strobj_init();
    registerobj_init();
    listobj_init();
    bytesobj_init();
    bitsobj_init();
    intobj_init();
    functionobj_init();
    dictobj_init();

    obj_init(&macro_obj, T_MACRO, "macro");
    macro_obj.destroy = macro_destroy;
    macro_obj.same = macro_same;
    obj_init(&segment_obj, T_SEGMENT, "segment");
    segment_obj.destroy = macro_destroy;
    segment_obj.same = macro_same;
    obj_init(&lbl_obj, T_LBL, "lbl");
    lbl_obj.same = lbl_same;
    obj_init(&mfunc_obj, T_MFUNC, "function");
    mfunc_obj.destroy = mfunc_destroy;
    mfunc_obj.same = mfunc_same;
    mfunc_obj.calc2 = mfunc_calc2;
    obj_init(&struct_obj, T_STRUCT, "struct");
    struct_obj.destroy = macro_destroy;
    struct_obj.same = macro_same;
    struct_obj.size = struct_size;
    struct_obj.calc2 = struct_calc2;
    obj_init(&union_obj, T_UNION, "union");
    union_obj.destroy = macro_destroy;
    union_obj.same = macro_same;
    union_obj.size = struct_size;
    union_obj.calc2 = struct_calc2;
    obj_init(&none_obj, T_NONE, "none");
    none_obj.truth = none_truth;
    none_obj.repr = none_repr;
    none_obj.hash = none_hash;
    none_obj.calc1 = none_calc1;
    none_obj.calc2 = none_calc2;
    none_obj.rcalc2 = none_rcalc2;
    none_obj.ival = none_ival;
    none_obj.uval = none_uval;
    none_obj.sign = none_sign;
    none_obj.abs = none_abs;
    none_obj.len = none_len;
    none_obj.size = none_size;
    obj_init(&error_obj, T_ERROR, "error");
    error_obj.destroy = error_destroy;
    error_obj.calc1 = error_calc1;
    error_obj.calc2 = error_calc2;
    error_obj.rcalc2 = error_rcalc2;
    obj_init(&gap_obj, T_GAP, "gap");
    gap_obj.create = gap_create;
    gap_obj.hash = gap_hash;
    gap_obj.repr = gap_repr;
    gap_obj.calc1 = gap_calc1;
    gap_obj.calc2 = gap_calc2;
    gap_obj.rcalc2 = gap_rcalc2;
    obj_init(&ident_obj, T_IDENT, "ident");
    ident_obj.calc2 = ident_calc2;
    ident_obj.rcalc2 = ident_rcalc2;
    obj_init(&anonident_obj, T_ANONIDENT, "anonident");
    anonident_obj.calc2 = ident_calc2;
    anonident_obj.rcalc2 = ident_rcalc2;
    obj_init(&oper_obj, T_OPER, "oper");
    oper_obj.repr = oper_repr;
    obj_init(&default_obj, T_DEFAULT, "default");
    obj_init(&iter_obj, T_ITER, "iter");
    iter_obj.destroy = iter_destroy;
    iter_obj.next = iter_next;
    obj_init(&funcargs_obj, T_FUNCARGS, "funcargs");
    funcargs_obj.same = funcargs_same;
    obj_init(&type_obj, T_TYPE, "type");
    type_obj.create = type_create;
    type_obj.same = type_same;
    type_obj.hash = type_hash;
    type_obj.repr = type_repr;
    type_obj.calc2 = type_calc2;
}

