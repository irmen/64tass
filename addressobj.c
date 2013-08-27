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
#include "addressobj.h"

#include "boolobj.h"
#include "intobj.h"
#include "strobj.h"

static struct obj_s obj;

obj_t ADDRESS_OBJ = &obj;

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = ADDRESS_OBJ;
    v->refcount = 1;
    v->u.addr.type = v1->u.addr.type;
    v->u.addr.val = v1->u.addr.val;
    v->u.addr.len = v1->u.addr.len;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == ADDRESS_OBJ && v1->u.addr.type == v2->u.addr.type && v1->u.addr.val == v2->u.addr.val && v1->u.addr.len == v2->u.addr.len;
}

static int truth(const struct value_s *v1) {
    return !!v1->u.addr.val;
}

static void repr(const struct value_s *v1, struct value_s *v) {
    uint8_t *s;
    size_t len;
    char buffer[100], buffer2[100];
    uint32_t addrtype;
    int ind, ind2;

    ind2 = sprintf(buffer,"$%" PRIxval, v1->u.addr.val);
    addrtype = v1->u.addr.type;
    ind = 99;
    buffer2[ind] = '\0';
    while (addrtype & 0xfff) {
        switch ((enum atype_e)((addrtype & 0xf00) >> 8)) {
        case A_NONE:break;
        case A_XR: buffer[ind2++] = ','; buffer[ind2++] = 'x';break;
        case A_YR: buffer[ind2++] = ','; buffer[ind2++] = 'y';break;
        case A_ZR: buffer[ind2++] = ','; buffer[ind2++] = 'z';break;
        case A_SR: buffer[ind2++] = ','; buffer[ind2++] = 's';break;
        case A_RR: buffer[ind2++] = ','; buffer[ind2++] = 'r';break;
        case A_I: buffer2[--ind] = '('; buffer[ind2++] = ')';break;
        case A_LI: buffer2[--ind] = '['; buffer[ind2++] = ']';break;
        case A_IMMEDIATE: buffer2[--ind] = '#';break;
        }
        addrtype <<= 4;
    }

    len = 99 - ind + ind2;

    if (v == v1) v->obj->destroy(v);
    v->obj = STR_OBJ;
    v->u.str.len = len;
    v->u.str.chars = len;
    s = (uint8_t *)malloc(v->u.str.len);
    if (!s) err_msg_out_of_memory();
    memcpy(s, buffer2 + ind, 99 - ind);
    memcpy(s + 99 - ind, buffer, ind2);
    v->u.str.data = s;
}

static inline int check_addr(uint32_t type) {
    while (type) {
        switch ((enum atype_e)(type & 15)) {
        case A_I:
        case A_LI:
        case A_IMMEDIATE: return 1;
        case A_XR:
        case A_YR:
        case A_ZR:
        case A_RR:
        case A_SR:
        case A_NONE: break;
        }
        type >>= 4;
    }
    return 0;
}

static void calc1(oper_t op) {
    struct value_s *v = op->v;
    address_t v1 = op->v1->u.addr.val;
    enum atype_e am;
    switch (op->op->u.oper.op) {
    case O_BANK: v1 >>= 8;
    case O_HIGHER: v1 >>= 8;
    case O_LOWER:
        if (op->v1->u.addr.type != A_IMMEDIATE) break;
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = (uint8_t)v1; 
        v->u.addr.len = 8; 
        v->u.addr.type = A_IMMEDIATE;
        return;
    case O_HWORD: v1 >>= 8;
    case O_WORD:
        if (op->v1->u.addr.type != A_IMMEDIATE) break;
        v->obj = ADDRESS_OBJ;
        v->u.addr.val = (uint16_t)v1;
        v->u.addr.len = 16; 
        v->u.addr.type = A_IMMEDIATE;
        return;
    case O_BSWORD:
        if (op->v1->u.addr.type != A_IMMEDIATE) break;
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = (uint8_t)(v1 >> 8) | (uint16_t)(v1 << 8);
        v->u.addr.len = 16;
        v->u.addr.type = A_IMMEDIATE;
        return;
    case O_COMMAS: am = A_SR; goto addr;
    case O_COMMAR: am = A_RR; goto addr;
    case O_COMMAZ: am = A_ZR; goto addr;
    case O_COMMAY: am = A_YR; goto addr;
    case O_COMMAX: am = A_XR; goto addr;
    case O_HASH: am = A_IMMEDIATE;
    addr:
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v1; 
        v->u.addr.len = op->v1->u.addr.len;
        v->u.addr.type = (op->v1->u.addr.type << 4) | am;
        return;
    case O_INV:
        if (op->v1->u.addr.type != A_IMMEDIATE) break;
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = ~v1;
        v->u.addr.len = op->v1->u.addr.len;
        v->u.addr.type = A_IMMEDIATE;
        return;
    case O_NEG:
        if (op->v1->u.addr.type != A_IMMEDIATE) break;
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = -v1;
        v->u.addr.len = op->v1->u.addr.len;
        v->u.addr.type = A_IMMEDIATE;
        return;
    case O_POS:
        if (op->v1->u.addr.type != A_IMMEDIATE) break;
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v1;
        v->u.addr.len = op->v1->u.addr.len;
        v->u.addr.type = A_IMMEDIATE;
        return;
    case O_LNOT: bool_from_int(v, !truth(op->v1)); return;
    case O_STRING: break;
    default: break;
    }
    obj_oper_error(op);
}

static int calc2_address(oper_t op, address_t v1, address_t v2) {
    struct value_s *v = op->v;
    switch (op->op->u.oper.op) {
    case O_CMP: int_from_int(v, (v1 > v2) - (v1 < v2)); return 0;
    case O_EQ: bool_from_int(v, v1 == v2); return 0;
    case O_NE: bool_from_int(v, v1 != v2); return 0;
    case O_LT: bool_from_int(v, v1 < v2); return 0;
    case O_LE: bool_from_int(v, v1 <= v2); return 0;
    case O_GT: bool_from_int(v, v1 > v2); return 0;
    case O_GE: bool_from_int(v, v1 >= v2); return 0;
    case O_ADD: 
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v1 + v2; return 0;
    case O_SUB: 
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v1 - v2; return 0;
    case O_MUL:
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v1 * v2; return 0;
    case O_DIV:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v1 / v2; return 0;
    case O_MOD:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v1 % v2; return 0;
    default: break;
    }
    return 1;
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    struct value_s tmp;
    ival_t ival;
    switch (v2->obj->type) {
    case T_ADDRESS:
        if (v1->u.addr.type != A_IMMEDIATE || v2->u.addr.type != A_IMMEDIATE) break;
        if (calc2_address(op, v1->u.addr.val, v2->u.addr.val)) break; 
        if (v->obj == ADDRESS_OBJ) {
            v->u.addr.type = A_IMMEDIATE;
            v->u.addr.len = 8*sizeof(address_t);
        }
        return;
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT: 
    case T_CODE: 
        switch (op->op->u.oper.op) {
        case O_ADD:
            if (check_addr(v1->u.addr.type)) break;
            if (v2->obj->ival(v2, &tmp, &ival, 8*sizeof(ival_t), &op->epoint2)) {
                if (v1 == v || v2 == v) v->obj->destroy(v);
                tmp.obj->copy_temp(&tmp, v);
                return;
            }
            if (ival >= 0) {
                if (calc2_address(op, v1->u.addr.val, ival)) break;
            } else {
                op->op = &o_SUB;
                if (calc2_address(op, v1->u.addr.val, -ival)) break;
                op->op = &o_ADD;
            }
            v->u.addr.type = v1->u.addr.type;
            v->u.addr.len = v1->u.addr.len; return;
        case O_SUB:
            if (check_addr(v1->u.addr.type)) break;
            if (v2->obj->ival(v2, &tmp, &ival, 8*sizeof(ival_t), &op->epoint2)) {
                if (v1 == v || v2 == v) v->obj->destroy(v);
                tmp.obj->copy_temp(&tmp, v);
            }
            if (ival >= 0) {
                if (calc2_address(op, v1->u.addr.val, ival)) break;
            } else {
                op->op = &o_ADD;
                if (calc2_address(op, v1->u.addr.val, -ival)) break;
                op->op = &o_SUB;
            }
            v->u.addr.type = v1->u.addr.type;
            v->u.addr.len = v1->u.addr.len; return;
        default: break;
        }
        break;
    default: v2->obj->rcalc2(op); return;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    ival_t ival;
    struct value_s tmp;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;
    switch (v1->obj->type) {
    case T_ADDRESS:
        if (v1->u.addr.type != A_IMMEDIATE || v2->u.addr.type != A_IMMEDIATE) break;
        if (calc2_address(op, v1->u.addr.val, v2->u.addr.val)) break;
        if (v->obj == ADDRESS_OBJ) {
            v->u.addr.type = A_IMMEDIATE;
            v->u.addr.len = 8*sizeof(address_t);
        }
        return;
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT: 
    case T_CODE: 
        if (op->op != &o_ADD || check_addr(v2->u.addr.type)) break;
        if (v1->obj->ival(v1, &tmp, &ival, 8*sizeof(ival_t), &op->epoint)) {
            if (v1 == v || v2 == v) v->obj->destroy(v);
            tmp.obj->copy_temp(&tmp, v);
        }
        if (calc2_address(op, ival, v2->u.addr.val)) break;
        v->u.addr.type = v2->u.addr.type;
        v->u.addr.len = v2->u.addr.len;
        return;
    default:
        if (op->op != &o_IN) {
            v1->obj->calc2(op); return;
        }
    }
    obj_oper_error(op);
}

void addressobj_init(void) {
    obj_init(&obj, T_ADDRESS, "<address>");
    obj.copy = copy;
    obj.copy_temp = copy;
    obj.same = same;
    obj.truth = truth;
    obj.repr = repr;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
