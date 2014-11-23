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
#include "boolobj.h"
#include "floatobj.h"

static struct obj_s obj;

obj_t BOOL_OBJ = &obj;

static MUST_CHECK value_t create(const value_t v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_BOOL: return val_reference(v1);
    default: return v1->obj->truth(v1, TRUTH_BOOL, epoint);
    }
}

static int same(const value_t v1, const value_t v2) {
    return v2->obj == BOOL_OBJ && v1->u.boolean == v2->u.boolean;
}

static MUST_CHECK value_t truth(const value_t v1, enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    return truth_reference(v1->u.boolean);
}

static MUST_CHECK value_t hash(const value_t v1, int *hs, linepos_t UNUSED(epoint)) {
    *hs = v1->u.boolean;
    return NULL;
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t UNUSED(epoint)) {
    value_t v = val_alloc(STR_OBJ);
    uint8_t *s = str_create_elements(v, 4 + !v1->u.boolean);
    v->u.str.data = s;
    v->u.str.len = 4 + !v1->u.boolean;
    v->u.str.chars = v->u.str.len;
    memcpy(s, v1->u.boolean ? "true" : "false", v->u.str.len);
    return v;
}

static MUST_CHECK value_t ival(const value_t v1, ival_t *iv, int UNUSED(bits), linepos_t UNUSED(epoint)) {
    *iv = v1->u.boolean;
    return NULL;
}

static MUST_CHECK value_t uval(const value_t v1, uval_t *uv, int UNUSED(bits), linepos_t UNUSED(epoint)) {
    *uv = v1->u.boolean;
    return NULL;
}

MUST_CHECK value_t float_from_bool(const value_t v1) {
    return float_from_double(v1->u.boolean);
}

MUST_CHECK value_t int_from_bool(const value_t v1) {
    return val_reference(int_value[v1->u.boolean]);
}

static inline MUST_CHECK value_t int_from_bool2(int i) {
    return val_reference(int_value[i]);
}

static MUST_CHECK value_t sign(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_bool(v1);
}

static MUST_CHECK value_t absolute(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_bool(v1);
}

static MUST_CHECK value_t calc1(oper_t op) {
    value_t v1 = op->v1, v;
    switch (op->op->u.oper.op) {
    case O_BANK:
    case O_HIGHER: return bytes_from_u8(0);
    case O_LOWER: return bytes_from_u8(v1->u.boolean);
    case O_HWORD: return bytes_from_u16(0);
    case O_WORD: return bytes_from_u16(v1->u.boolean);
    case O_BSWORD: return bytes_from_u16(v1->u.boolean << 8);
    case O_INV: return ibits_from_bool(v1->u.boolean);
    case O_NEG: return int_from_int(-v1->u.boolean);
    case O_POS: return int_from_bool(v1);
    case O_STRING:
        v = val_alloc(STR_OBJ);
        v->u.str.data = v->u.str.val;
        v->u.str.len = 1;
        v->u.str.chars = 1;
        v->u.str.val[0] = v1->u.boolean ? '1' : '0';
        return v;
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t calc2_bool(oper_t op, int v1, int v2) {
    switch (op->op->u.oper.op) {
    case O_CMP: 
        if (!v1 && v2) return int_from_int(-1);
        return val_reference(int_value[v1 - v2]);
    case O_EQ: return truth_reference(v1 == v2);
    case O_NE: return truth_reference(v1 != v2);
    case O_LT: return truth_reference(v1 < v2);
    case O_LE: return truth_reference(v1 <= v2);
    case O_GT: return truth_reference(v1 > v2);
    case O_GE: return truth_reference(v1 >= v2);
    case O_ADD: return int_from_int(v1 + v2);
    case O_SUB: return int_from_int(v1 - v2);
    case O_MUL: return int_from_bool2(v1 & v2);
    case O_DIV:
        if (!v2) { 
            return new_error_obj(ERROR_DIVISION_BY_Z, op->epoint3); 
        }
        return int_from_bool2(v1);
    case O_MOD:
        if (!v2) { 
            return new_error_obj(ERROR_DIVISION_BY_Z, op->epoint3);
        }
        return int_from_bool2(0);
    case O_EXP: return int_from_bool2(v1 | !v1);
    case O_AND: return truth_reference(v1 & v2);
    case O_OR: return truth_reference(v1 | v2);
    case O_XOR: return truth_reference(v1 ^ v2);
    case O_LSHIFT: return v2 ? bits_from_bools(v1, 0) : bits_from_bool(v1);
    case O_RSHIFT: return v2 ? val_reference(null_bits) : bits_from_bool(v1);
    case O_CONCAT: return bits_from_bools(v1, v2);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_BOOL: return calc2_bool(op, op->v1->u.boolean, op->v2->u.boolean);
    default: 
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return op->v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    switch (op->v1->obj->type) {
    case T_BOOL: return calc2_bool(op, op->v1->u.boolean, op->v2->u.boolean);
    default:
        if (op->op != &o_IN) {
            return op->v1->obj->calc2(op);
        }
    }
    return obj_oper_error(op);
}

void boolobj_init(void) {
    obj_init(&obj, T_BOOL, "bool");
    obj.create = create;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.repr = repr;
    obj.ival = ival;
    obj.uval = uval;
    obj.sign = sign;
    obj.abs = absolute;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
