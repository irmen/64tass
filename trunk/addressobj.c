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

static struct obj_s obj;

obj_t ADDRESS_OBJ = &obj;

static void destroy(value_t v1) {
    val_destroy(v1->u.addr.val);
}

static int same(const value_t v1, const value_t v2) {
    return v2->obj == ADDRESS_OBJ && v1->u.addr.type == v2->u.addr.type && obj_same(v1->u.addr.val, v2->u.addr.val);
}

static inline MUST_CHECK value_t type_check(const value_t v1, enum errors_e num, linepos_t epoint) {
    if (v1->u.addr.type != A_NONE) {
        value_t v = val_alloc(ERROR_OBJ);
        v->u.error.num = num;
        v->u.error.epoint = *epoint;
        v->u.error.u.objname = v1->obj->name;
        return v;
    }
    return NULL;
}

static MUST_CHECK value_t truth(value_t v1, enum truth_e type, linepos_t epoint) {
    value_t err = type_check(v1, ERROR_____CANT_BOOL, epoint);
    if (err) return err;
    v1 = v1->u.addr.val;
    return v1->obj->truth(v1, type, epoint);
}

static MUST_CHECK value_t ival(value_t v1, ival_t *iv, int bits, linepos_t epoint) {
    value_t err = type_check(v1, ERROR______CANT_INT, epoint);
    if (err) return err;
    v1 = v1->u.addr.val;
    return v1->obj->ival(v1, iv, bits, epoint);
}

static MUST_CHECK value_t uval(value_t v1, uval_t *uv, int bits, linepos_t epoint) {
    value_t err = type_check(v1, ERROR______CANT_INT, epoint);
    if (err) return err;
    v1 = v1->u.addr.val;
    return v1->obj->uval(v1, uv, bits, epoint);
}

static MUST_CHECK value_t real(value_t v1, double *r, linepos_t epoint) {
    value_t err = type_check(v1, ERROR_____CANT_REAL, epoint);
    if (err) return err;
    v1 = v1->u.addr.val;
    return v1->obj->real(v1, r, epoint);
}

static MUST_CHECK value_t sign(value_t v1, linepos_t epoint) {
    value_t err = type_check(v1, ERROR_____CANT_SIGN, epoint);
    if (err) return err;
    v1 = v1->u.addr.val;
    return v1->obj->sign(v1, epoint);
}

static MUST_CHECK value_t absolute(value_t v1, linepos_t epoint) {
    value_t err = type_check(v1, ERROR______CANT_ABS, epoint);
    if (err) return err;
    v1 = v1->u.addr.val;
    return v1->obj->abs(v1, epoint);
}

static MUST_CHECK value_t integer(value_t v1, linepos_t epoint) {
    value_t err = type_check(v1, ERROR______CANT_INT, epoint);
    if (err) return err;
    v1 = v1->u.addr.val;
    return v1->obj->integer(v1, epoint);
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t epoint) {
    uint8_t *s;
    size_t len;
    char buffer[100], buffer2[100];
    atype_t addrtype;
    int ind, ind2;
    value_t tmp, v;

    tmp = v1->u.addr.val->obj->repr(v1->u.addr.val, epoint);
    if (tmp->obj != STR_OBJ) return tmp;
    ind2 = 0;
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
        case A_DR: buffer[ind2++] = ','; buffer[ind2++] = 'd';break;
        case A_BR: buffer[ind2++] = ','; buffer[ind2++] = 'b';break;
        case A_KR: buffer[ind2++] = ','; buffer[ind2++] = 'k';break;
        case A_I: buffer2[--ind] = '('; buffer[ind2++] = ')';break;
        case A_LI: buffer2[--ind] = '['; buffer[ind2++] = ']';break;
        case A_IMMEDIATE: buffer2[--ind] = '#';break;
        }
        addrtype <<= 4;
    }

    len = 99 - ind + ind2;

    v = val_alloc(STR_OBJ);
    v->u.str.len = len + tmp->u.str.len;
    v->u.str.chars = len + tmp->u.str.chars;
    if (v->u.str.len < len) err_msg_out_of_memory(); /* overflow */
    s = str_create_elements(v, v->u.str.len);
    memcpy(s, buffer2 + ind, 99 - ind);
    memcpy(s + 99 - ind, tmp->u.str.data, tmp->u.str.len);
    memcpy(s + 99 - ind + tmp->u.str.len, buffer, ind2);
    v->u.str.data = s;
    val_destroy(tmp);
    return v;
}

static inline int check_addr(atype_t type) {
    while (type) {
        switch ((enum atype_e)(type & 15)) {
        case A_I:
        case A_LI:
        case A_IMMEDIATE: return 1;
        case A_KR:
        case A_DR:
        case A_BR:
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

static MUST_CHECK value_t calc1(oper_t op) {
    value_t v, v1 = op->v1;
    atype_t am;
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
        am = v1->u.addr.type;
        if (am != A_IMMEDIATE && am != A_NONE) break;
        op->v1 = v1->u.addr.val;
        v = val_alloc(ADDRESS_OBJ);
        v->u.addr.type = am;
        v->u.addr.val = op->v1->obj->calc1(op);
        if (v->u.addr.val->obj == ERROR_OBJ) { err_msg_output_and_destroy(v->u.addr.val); v->u.addr.val = val_reference(none_value); }
        op->v1 = v1;
        return v;
    case O_STRING: break;
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    value_t result;
    atype_t am;
    switch (v2->obj->type) {
    case T_ADDRESS:
        switch (op->op->u.oper.op) {
        case O_CMP:
            if (v1->u.addr.type == v2->u.addr.type) {
                op->v1 = v1->u.addr.val;
                op->v2 = v2->u.addr.val;
                result = op->v1->obj->calc2(op);
                op->v1 = v1;
                op->v2 = v2;
                return result;
            } 
            return int_from_int((v1->u.addr.type > v2->u.addr.type) - (v1->u.addr.type < v2->u.addr.type));
        case O_EQ:
        case O_NE:
        case O_LT:
        case O_LE:
        case O_GT:
        case O_GE: 
            if (v1->u.addr.type == v2->u.addr.type) {
                op->v1 = v1->u.addr.val;
                op->v2 = v2->u.addr.val;
                result = op->v1->obj->calc2(op);
                op->v1 = v1;
                op->v2 = v2;
                return result;
            } 
            {
                int r;
                switch (op->op->u.oper.op) {
                case O_EQ: r = (v1->u.addr.type == v2->u.addr.type); break;
                case O_NE: r = (v1->u.addr.type != v2->u.addr.type); break;
                case O_LT: r = (v1->u.addr.type < v2->u.addr.type); break;
                case O_LE: r = (v1->u.addr.type <= v2->u.addr.type); break;
                case O_GT: r = (v1->u.addr.type > v2->u.addr.type); break;
                case O_GE: r = (v1->u.addr.type >= v2->u.addr.type); break;
                default: r = 0; break; /* can't happen */
                }
                return truth_reference(r);
            }
        default:
            am = v1->u.addr.type;
            if (am != A_IMMEDIATE || v2->u.addr.type != A_IMMEDIATE) 
                if (am != A_NONE || v2->u.addr.type != A_NONE) {
                    return obj_oper_error(op);
                }
            op->v1 = v1->u.addr.val;
            op->v2 = v2->u.addr.val;
            v = val_alloc(ADDRESS_OBJ);
            v->u.addr.val = op->v1->obj->calc2(op);
            if (v->u.addr.val->obj == ERROR_OBJ) { err_msg_output_and_destroy(v->u.addr.val); v->u.addr.val = val_reference(none_value); }
            v->u.addr.type = am;
            op->v1 = v1;
            op->v2 = v2;
            return v;
        }
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT: 
    case T_CODE: 
        am = v1->u.addr.type;
        if (check_addr(am)) break;
        op->v1 = v1->u.addr.val;
        v = val_alloc(ADDRESS_OBJ);
        v->u.addr.val = op->v1->obj->calc2(op);
        if (v->u.addr.val->obj == ERROR_OBJ) { err_msg_output_and_destroy(v->u.addr.val); v->u.addr.val = val_reference(none_value); }
        v->u.addr.type = am;
        op->v1 = v1;
        return v;
    default: 
        if (op->op != &o_MEMBER) {
            return v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    atype_t am;
    switch (v1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT: 
    case T_CODE: 
        am = v2->u.addr.type;
        if (check_addr(am)) break;
        op->v2 = v2->u.addr.val;
        v = val_alloc(ADDRESS_OBJ);
        v->u.addr.val = op->v2->obj->rcalc2(op);
        if (v->u.addr.val->obj == ERROR_OBJ) { err_msg_output_and_destroy(v->u.addr.val); v->u.addr.val = val_reference(none_value); }
        v->u.addr.type = am;
        op->v2 = v2;
        return v;
    default:
    case T_ADDRESS:
        if (op->op != &o_IN) {
            return v1->obj->calc2(op);
        }
    }
    return obj_oper_error(op);
}

void addressobj_init(void) {
    obj_init(&obj, T_ADDRESS, "<address>");
    obj.destroy = destroy;
    obj.same = same;
    obj.truth = truth;
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
