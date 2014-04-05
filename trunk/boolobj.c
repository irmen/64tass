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

static struct obj_s obj;

obj_t BOOL_OBJ = &obj;

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = BOOL_OBJ;
    v->u.boolean = v1->u.boolean;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == BOOL_OBJ && v1->u.boolean == v2->u.boolean;
}

static int truth(const struct value_s *v1, struct value_s *v, enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    if (v1 != v) bool_from_int(v, v1->u.boolean);
    return 0;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    return v1->u.boolean;
}

static void repr(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    uint8_t *s = str_create_elements(v, 4 + !v1->u.boolean);
    v->obj = STR_OBJ;
    v->u.str.data = s;
    v->u.str.len = 4 + !v1->u.boolean;
    v->u.str.chars = v->u.str.len;
    memcpy(s, v1->u.boolean ? "true" : "false", v->u.str.len);
    return;
}

static int MUST_CHECK ival(const struct value_s *v1, struct value_s *UNUSED(v), ival_t *iv, int UNUSED(bits), linepos_t UNUSED(epoint)) {
    *iv = v1->u.boolean;
    return 0;
}

static int MUST_CHECK uval(const struct value_s *v1, struct value_s *UNUSED(v), uval_t *uv, int UNUSED(bits), linepos_t UNUSED(epoint)) {
    *uv = v1->u.boolean;
    return 0;
}

static int MUST_CHECK real(const struct value_s *v1, struct value_s *UNUSED(v), double *r, linepos_t UNUSED(epoint)) {
    *r = v1->u.boolean;
    return 0;
}

static void sign(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    int_from_int(v, v1->u.boolean);
}

static void absolute(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    int_from_int(v, v1->u.boolean);
}

static void integer(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    int_from_int(v, v1->u.boolean);
}

static void calc1(oper_t op) {
    struct value_s *v = op->v, *v1 = op->v1;
    switch (op->op->u.oper.op) {
    case O_BANK:
    case O_HIGHER:
        bits_from_u8(v, 0);
        return;
    case O_LOWER:
        bits_from_u8(v, v1->u.boolean);
        return;
    case O_HWORD:
        bits_from_u16(v, 0);
        return;
    case O_WORD:
        bits_from_u16(v, v1->u.boolean);
        return;
    case O_BSWORD:
        bits_from_u16(v, v1->u.boolean << 8);
        return;
    case O_INV:
        int_from_int(v, ~v1->u.boolean);
        return;
    case O_NEG:
        int_from_int(v, -v1->u.boolean);
        return;
    case O_POS:
        int_from_int(v, v1->u.boolean);
        return;
    case O_STRING: repr(v1, v, &op->epoint);break;
    default: break;
    }
    obj_oper_error(op);
}

static MUST_CHECK struct value_s *calc2_bool(oper_t op, int v1, int v2) {
    struct value_s *v = op->v;
    switch (op->op->u.oper.op) {
    case O_CMP: int_from_int(v, v1 - v2); return NULL;
    case O_EQ: return truth_reference(v1 == v2);
    case O_NE: return truth_reference(v1 != v2);
    case O_LT: return truth_reference(v1 < v2);
    case O_LE: return truth_reference(v1 <= v2);
    case O_GT: return truth_reference(v1 > v2);
    case O_GE: return truth_reference(v1 >= v2);
    case O_ADD: int_from_int(v, v1 + v2); return NULL;
    case O_SUB: int_from_int(v, v1 - v2); return NULL;
    case O_MUL: int_from_int(v, v1 & v2); return NULL;
    case O_DIV:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        int_from_int(v, v1); return NULL;
    case O_MOD:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        int_from_int(v, 0); return NULL;
    case O_EXP: int_from_int(v, v1 | !v1); return NULL;
    case O_AND: return truth_reference(v1 & v2);
    case O_OR: return truth_reference(v1 | v2);
    case O_XOR: return truth_reference(v1 ^ v2);
    case O_LSHIFT: int_from_int(v, v1 << v2); return NULL;
    case O_RSHIFT: int_from_int(v, v1 >> v2); return NULL;
    case O_CONCAT: bits_from_bools(v, v1, v2); return NULL;
    default: break;
    }
    obj_oper_error(op);
    return NULL;
}

static MUST_CHECK struct value_s *calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_BOOL: return calc2_bool(op, op->v1->u.boolean, op->v2->u.boolean);
    default: 
        if (op->op != &o_MEMBER) {
            return op->v2->obj->rcalc2(op);
        }
    }
    obj_oper_error(op);
    return NULL;
}

static MUST_CHECK struct value_s *rcalc2(oper_t op) {
    switch (op->v1->obj->type) {
    case T_BOOL: return calc2_bool(op, op->v1->u.boolean, op->v2->u.boolean);
    default:
        if (op->op != &o_IN) {
            return op->v1->obj->calc2(op);
        }
    }
    obj_oper_error(op);
    return NULL;
}

void boolobj_init(void) {
    obj_init(&obj, T_BOOL, "<bool>");
    obj.copy = copy;
    obj.copy_temp = copy;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.repr = repr;
    obj.ival = ival;
    obj.uval = uval;
    obj.real = real;
    obj.sign = sign;
    obj.abs = absolute;
    obj.integer = integer;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
