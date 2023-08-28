/*
    $Id: boolobj.c 3045 2023-08-21 06:38:49Z soci $

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
#include "boolobj.h"
#include <string.h>
#include "eval.h"
#include "error.h"
#include "variables.h"
#include "arguments.h"

#include "floatobj.h"
#include "strobj.h"
#include "bitsobj.h"
#include "intobj.h"
#include "typeobj.h"
#include "errorobj.h"
#include "noneobj.h"
#include "functionobj.h"

static Type obj;

Type *const BOOL_OBJ = &obj;

static Bool trueval = { { &obj, 2 }, NULL, true };
static Bool falseval = { { &obj, 2 }, NULL, false };

Obj *const true_value = &trueval.v;
Obj *const false_value = &falseval.v;
Obj *const bool_value[2] = { &falseval.v, &trueval.v };

static MUST_CHECK Obj *bool_from_obj(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_BOOL: return val_reference(v1);
    default:
        if (v1->obj->truth != DEFAULT_OBJ->truth) {
            return v1->obj->truth(v1, TRUTH_BOOL, epoint);
        }
        break;
    }
    return new_error_conv(v1, BOOL_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return bool_from_obj(op->v2, op->epoint2);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    return o1 == o2;
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types UNUSED(type), linepos_t UNUSED(epoint)) {
    return val_reference(o1);
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    *hs = Bool(o1)->value ? 1 : 0;
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Bool *v1 = Bool(o1);
    Str *v;
    size_t len = v1->value ? 4 : 5;
    if (len > maxsize) return NULL;
    v = v1->repr;
    if (v == NULL) {
        v = new_str2(len);
        if (v == NULL) return NULL;
        v->chars = len;
        memcpy(v->data, v1->value ? "true" : "false", len);
        v1->repr = v;
    }
    return val_reference(Obj(v));
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int UNUSED(bits), linepos_t epoint) {
    if (diagnostics.strict_bool) err_msg_bool(ERROR______CANT_INT, o1, epoint);
    *iv = Bool(o1)->value ? 1 : 0;
    return NULL;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int UNUSED(bits), linepos_t epoint) {
    if (diagnostics.strict_bool) err_msg_bool(ERROR______CANT_INT, o1, epoint);
    *uv = Bool(o1)->value ? 1 : 0;
    return NULL;
}

MUST_CHECK Obj *float_from_bool(const Bool *v1) {
    return new_float(v1->value ? 1.0 : 0.0);
}

MUST_CHECK Obj *int_from_bool(const Bool *v1) {
    return val_reference(int_value[v1->value ? 1 : 0]);
}

static inline MUST_CHECK Obj *int_from_bool2(bool i) {
    return val_reference(int_value[i ? 1 : 0]);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t epoint) {
    if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_SIGN, o1, epoint);
    return val_reference(int_value[Bool(o1)->value ? 1 : 0]);
}

static MUST_CHECK Obj *function(oper_t op) {
    if (diagnostics.strict_bool) err_msg_bool((Function(op->v1)->func == F_ABS) ? ERROR______CANT_ABS : ERROR______CANT_INT, op->v2, op->epoint2);
    return val_reference(int_value[Bool(op->v2)->value ? 1 : 0]);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    bool v1 = Bool(op->v1)->value;
    Str *v;
    if (diagnostics.strict_bool && op->op != O_LNOT) err_msg_bool_oper(op);
    switch (op->op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
        return bits_calc1(op->op, v1 ? 1U : 0U);
    case O_INV: return val_reference(ibits_value[v1 ? 1 : 0]);
    case O_NEG: return val_reference(v1 ? ibits_value[0] : bits_value[0]);
    case O_POS: return val_reference(bits_value[v1 ? 1 : 0]);
    case O_STRING:
        v = new_str2(1);
        if (v == NULL) return NULL;
        v->chars = 1;
        v->data[0] = v1 ? '1' : '0';
        return Obj(v);
    case O_LNOT: return truth_reference(!v1);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2_bool(oper_t op) {
    bool v1 = Bool(op->v1)->value;
    bool v2 = Bool(op->v2)->value;
    switch (op->op) {
    case O_SUB:
    case O_CMP:
        if (!v1 && v2) return val_reference(minus1_value);
        return int_from_bool2(v1 != v2);
    case O_EQ: return truth_reference(v1 == v2);
    case O_NE: return truth_reference(v1 != v2);
    case O_MIN:
    case O_LT: return truth_reference(!v1 && v2);
    case O_LE: return truth_reference(!v1 || v2);
    case O_MAX:
    case O_GT: return truth_reference(v1 && !v2);
    case O_GE: return truth_reference(v1 || !v2);
    case O_ADD: return int_from_uval((v1 ? 1U : 0U) + (v2 ? 1U : 0U));
    case O_MUL: return int_from_bool2(v1 && v2);
    case O_DIV:
        if (!v2) {
            return new_error_obj(ERROR_DIVISION_BY_Z, op->v2, op->epoint2);
        }
        return int_from_bool2(v1);
    case O_MOD:
        if (!v2) {
            return new_error_obj(ERROR_DIVISION_BY_Z, op->v2, op->epoint2);
        }
        return int_from_bool2(false);
    case O_EXP: return int_from_bool2(v1 || !v2);
    case O_AND: return truth_reference(v1 && v2);
    case O_OR: return truth_reference(v1 || v2);
    case O_XOR: return truth_reference(v1 != v2);
    case O_LSHIFT: return v2 ? bits_from_uval(v1 ? 2 : 0, 2) : val_reference(bits_value[v1 ? 1 : 0]);
    case O_RSHIFT: return val_reference(v2 ? null_bits : bits_value[v1 ? 1 : 0]);
    case O_CONCAT: return bits_from_uval((v1 ? 2U : 0U) + (v2 ? 1U : 0U), 2);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *o1 = op->v1;
    Obj *o2 = op->v2;
    bool is_bool = o2->obj == BOOL_OBJ;

    if (op->op == O_LAND) {
        if (diagnostics.strict_bool && !is_bool) err_msg_bool_oper(op);
        return val_reference(Bool(o1)->value ? o2 : o1);
    }
    if (op->op == O_LOR) {
        if (diagnostics.strict_bool && !is_bool) err_msg_bool_oper(op);
        return val_reference(Bool(o1)->value ? o1 : o2);
    }
    if (op->op == O_LXOR) {
        if (diagnostics.strict_bool && !is_bool) err_msg_bool_oper(op);
        return calc2_lxor(op, Bool(o1)->value);
    }
    if (is_bool) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return calc2_bool(op);
    }
    if (op->op != O_MEMBER && op->op != O_X) {
        return o2->obj->rcalc2(op);
    }
    if (o2 == none_value || o2->obj == ERROR_OBJ) return val_reference(o2);
    return obj_oper_error(op);
}

void boolobj_init(void) {
    Type *type = new_type(&obj, T_BOOL, "bool", sizeof(Bool));
    type->convert = convert;
    type->same = same;
    type->truth = truth;
    type->hash = hash;
    type->repr = repr;
    type->ival = ival;
    type->uval = uval;
    type->uval2 = uval;
    type->iaddress = ival;
    type->uaddress = uval;
    type->sign = sign;
    type->function = function;
    type->calc1 = calc1;
    type->calc2 = calc2;
    falseval.v.refcount = 2;
    falseval.repr = NULL;
    trueval.v.refcount = 2;
    trueval.repr = NULL;
}

void boolobj_names(void) {
    new_builtin("bool", val_reference(Obj(BOOL_OBJ)));

    new_builtin("true", true_value);
    new_builtin("false", false_value);
}

void boolobj_destroy(void) {
#ifdef DEBUG
    if (false_value->refcount != 1) fprintf(stderr, "false %" PRIuSIZE "\n", false_value->refcount - 1);
    if (true_value->refcount != 1) fprintf(stderr, "true %" PRIuSIZE "\n", true_value->refcount - 1);
    if (falseval.repr != NULL && falseval.repr->v.refcount != 1) fprintf(stderr, "boolrepr[0] %" PRIuSIZE "\n", falseval.repr->v.refcount - 1);
    if (trueval.repr != NULL && trueval.repr->v.refcount != 1) fprintf(stderr, "boolrepr[1] %" PRIuSIZE "\n", trueval.repr->v.refcount - 1);
#endif

    if (falseval.repr != NULL) val_destroy(Obj(falseval.repr));
    if (trueval.repr != NULL) val_destroy(Obj(trueval.repr));
}
