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
#include "values.h"
#include "boolobj.h"

#include "intobj.h"
#include "strobj.h"
#include "addressobj.h"

static struct obj_s obj;

obj_t BOOL_OBJ = &obj;

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = BOOL_OBJ;
    v->refcount = 1;
    v->u.boolean = v1->u.boolean;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == BOOL_OBJ && v1->u.boolean == v2->u.boolean;
}

static int truth(const struct value_s *v1) {
    return v1->u.boolean;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    return v1->u.boolean;
}

static void repr(const struct value_s *v1, struct value_s *v) {
    uint8_t *s = (uint8_t *)malloc(1);
    if (!s) err_msg_out_of_memory();
    *s = v1->u.boolean + '0';
    v->obj = STR_OBJ;
    v->u.str.data = s;
    v->u.str.len = 1;
    v->u.str.chars = 1;
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

static int MUST_CHECK sign(const struct value_s *v1, struct value_s *UNUSED(v), int *s, linepos_t UNUSED(epoint)) {
    *s = v1->u.boolean;
    return 0;
}

static void absolute(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    int_from_int(v, v1->u.boolean);
}

static void integer(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    int_from_int(v, v1->u.boolean);
}

static void calc1(oper_t op) {
    struct value_s *v = op->v, *v1 = op->v1, *v2;
    enum atype_e am;
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
    case O_COMMAS: am = A_SR; goto addr;
    case O_COMMAR: am = A_RR; goto addr;
    case O_COMMAZ: am = A_ZR; goto addr;
    case O_COMMAY: am = A_YR; goto addr;
    case O_COMMAX: am = A_XR; goto addr;
    case O_HASH: am = A_IMMEDIATE;
    addr:
        if (v == v1) {
            v2 = val_alloc();
            copy(v1, v2);
        } else v2 = val_reference(v1);
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v2;
        v->u.addr.type = am;
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
    case O_LNOT: bool_from_int(v, !truth(v1)); return;
    case O_STRING: repr(v1, v);break;
    default: break;
    }
    obj_oper_error(op);
}

static int calc2_bool_bool(oper_t op, int v1, int v2) {
    struct value_s *v = op->v;
    switch (op->op->u.oper.op) {
    case O_CMP: int_from_int(v, v1 - v2); return 0;
    case O_EQ: bool_from_int(v, v1 == v2); return 0;
    case O_NE: bool_from_int(v, v1 != v2); return 0;
    case O_LT: bool_from_int(v, v1 < v2); return 0;
    case O_LE: bool_from_int(v, v1 <= v2); return 0;
    case O_GT: bool_from_int(v, v1 > v2); return 0;
    case O_GE: bool_from_int(v, v1 >= v2); return 0;
    case O_ADD: int_from_int(v, v1 + v2); return 0;
    case O_SUB: int_from_int(v, v1 - v2); return 0;
    case O_MUL: int_from_int(v, v1 & v2); return 0;
    case O_DIV:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        int_from_int(v, v1); return 0;
    case O_MOD:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        int_from_int(v, 0); return 0;
    case O_EXP: int_from_int(v, v1 | !v1); return 0;
    case O_AND: bool_from_int(v, v1 & v2); return 0;
    case O_OR: bool_from_int(v, v1 | v2); return 0;
    case O_XOR: bool_from_int(v, v1 ^ v2); return 0;
    case O_LSHIFT: int_from_int(v, v1 << v2); return 0;
    case O_RSHIFT: int_from_int(v, v1 >> v2); return 0;
    case O_CONCAT: bits_from_bools(v, v1, v2); return 0;
    default: break;
    }
    return 1;
}

static void calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_BOOL: if (calc2_bool_bool(op, op->v1->u.boolean, op->v2->u.boolean)) break; return;
    default: 
        if (op->op != &o_MEMBER) {
            op->v2->obj->rcalc2(op); return;
        }
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    switch (op->v1->obj->type) {
    case T_BOOL: if (calc2_bool_bool(op, op->v1->u.boolean, op->v2->u.boolean)) break; return;
    default:
        if (op->op != &o_IN) {
            op->v1->obj->calc2(op); return;
        }
    }
    obj_oper_error(op);
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
