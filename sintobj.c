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
#include <math.h>
#include "values.h"
#include "sintobj.h"

#include "numobj.h"
#include "strobj.h"
#include "boolobj.h"
#include "uintobj.h"
#include "floatobj.h"
#include "addressobj.h"

static struct obj_s obj;

obj_t SINT_OBJ = &obj;

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = v1->obj;
    v->refcount = 1;
    v->u.num.val = v1->u.num.val;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == SINT_OBJ && v1->u.num.val == v2->u.num.val;
}

static int truth(const struct value_s *v1) {
    return !!v1->u.num.val;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    return v1->u.num.val & ((~(unsigned int)0) >> 1);
}

static void convert(struct value_s *v1, struct value_s *v, obj_t t, linepos_t UNUSED(epoint), linepos_t UNUSED(epoint2)) {
    if (t == STR_OBJ) {
        char line[100]; 
        uint8_t *s;
        v->obj = t;
        v->u.str.len = sprintf(line, "%" PRIdval, v1->u.num.val);
        v->u.str.chars = v->u.str.len;
        s = malloc(v->u.str.len);
        if (!s) err_msg_out_of_memory();
        memcpy(s, line, v->u.str.len);
        v->u.str.data = s;
        return;
    }
    ///////////////////////////////////obj_oper_error(O_STRING, v1, NULL, v, epoint);
}

int calc1_sint(oper_t op, ival_t v1) {
    struct value_s *v;
    enum atype_e am;
    if (v1 >= 0) return calc1_uint(op, v1);
    v = op->v;
    switch (op->op->u.oper.op) {
    case O_BANK: v1 >>= 8;
    case O_HIGHER: v1 >>= 8;
    case O_LOWER:
        v->obj = NUM_OBJ; 
        v->u.num.val = (uint8_t)v1; 
        v->u.num.len = 8;
        return 0;
    case O_HWORD: v1 >>= 8;
    case O_WORD:
        v->obj = NUM_OBJ; 
        v->u.num.val = (uint16_t)v1; 
        v->u.num.len = 16; 
        return 0;
    case O_BSWORD:
        v->obj = NUM_OBJ; 
        v->u.num.val = (uint8_t)(v1 >> 8) | (uint16_t)(v1 << 8); 
        v->u.num.len = 16; 
        return 0;
    case O_COMMAS: am =  A_SR; goto addr;
    case O_COMMAR: am =  A_RR; goto addr;
    case O_COMMAZ: am =  A_ZR; goto addr;
    case O_COMMAY: am =  A_YR; goto addr;
    case O_COMMAX: am =  A_XR; goto addr;
    case O_HASH: am = A_IMMEDIATE;
    addr:
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v1; 
        v->u.addr.len = 8 * sizeof(address_t);
        v->u.addr.type = am; 
        return 0;
    case O_INV:
        v->obj = UINT_OBJ;
        v->u.num.val = ~v1;
        return 0;
    case O_NEG:
        v->obj = UINT_OBJ;
        v->u.num.val = -v1;
        return 0;
    case O_POS:
        v->obj = SINT_OBJ;
        v->u.num.val = v1;
        return 0;
    case O_LNOT:
        v->obj = BOOL_OBJ; 
        v->u.num.val = !v1;
        return 0;
    case O_STRING:
        v->u.num.val = v1;
        convert(v, v, STR_OBJ, &op->epoint, &op->epoint3);
        return 0;
    default: return 1;
    }
}

static void calc1(oper_t op) {
    if (!calc1_sint(op, op->v1->u.num.val)) return;
    obj_oper_error(op);
}

int calc2_sint_uint(oper_t op, ival_t v1, uval_t v2) {
    struct value_s *v;
    if (v1 >= 0) return calc2_uint_uint(op, v1, v2);
    v = op->v;
    switch (op->op->u.oper.op) {
    case O_CMP: v->obj = SINT_OBJ; v->u.num.val = -1; return 0;
    case O_EQ: v->obj = BOOL_OBJ; v->u.num.val = 0; return 0;
    case O_NE: v->obj = BOOL_OBJ; v->u.num.val = 1; return 0;
    case O_LT: v->obj = BOOL_OBJ; v->u.num.val = 1; return 0;
    case O_LE: v->obj = BOOL_OBJ; v->u.num.val = 1; return 0;
    case O_GT: v->obj = BOOL_OBJ; v->u.num.val = 0; return 0;
    case O_GE: v->obj = BOOL_OBJ; v->u.num.val = 0; return 0;
    case O_ADD: 
        v->obj = ((uval_t)-v1 > v2) ? SINT_OBJ : UINT_OBJ;  
        v->u.num.val = v1 + v2; return 0;
    case O_SUB: 
        v->obj = SINT_OBJ;
        v->u.num.val = v1 - v2; return 0;
    case O_MUL:
        v->obj = SINT_OBJ; 
        v->u.num.val = v1 * v2; return 0;
    case O_DIV:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        v->obj = SINT_OBJ; 
        v->u.num.val = -((uval_t)-v1 / v2); return 0;
    case O_MOD:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        v->obj = UINT_OBJ; 
        v->u.num.val = v2 - ((uval_t)-v1 % v2); return 0;
    case O_EXP:
        {
            ival_t val = 1;
            while (v2) {
                if (v2 & 1) val *= v1;
                v1 *= v1;
                v2 >>= 1;
            }
            v->obj = SINT_OBJ; 
            v->u.num.val = val; return 0;
        }
        break;
    case O_AND:
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 & v2; return 0;
    case O_OR:
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 | v2; return 0;
    case O_XOR:
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 ^ v2; return 0;
    case O_LSHIFT:
        if (v2 > 8 * sizeof(v1)) {
            v->obj = UINT_OBJ; 
            v->u.num.val = 0; return 0;
        }
        v->obj = SINT_OBJ; 
        v->u.num.val = -((uval_t)-v1 << v2); return 0;
    case O_RSHIFT:
        if (v2 > 8 * sizeof(v1)) {
            v->obj = UINT_OBJ; 
            v->u.num.val = 0; return 0;
        }
        v->obj = SINT_OBJ; 
        v->u.num.val = ~((uval_t)~v1 >> v2); return 0;
    default: break;
    }
    return 1;
}

static int calc2_sint_sint(oper_t op, ival_t v1, ival_t v2) {
    struct value_s *v;
    if (v1 >= 0) {
        if (v2 >= 0) return calc2_uint_uint(op, v1, v2);
        return calc2_uint_sint(op, v1, v2);
    }
    if (v2 >= 0) return calc2_sint_uint(op, v1, v2);
    v = op->v;
    switch (op->op->u.oper.op) {
    case O_CMP: v->obj = (v1 < v2) ? SINT_OBJ : UINT_OBJ; v->u.num.val = (v1 > v2) - (v1 < v2); return 0;
    case O_EQ: v->obj = BOOL_OBJ; v->u.num.val = (v1 == v2); return 0;
    case O_NE: v->obj = BOOL_OBJ; v->u.num.val = (v1 != v2); return 0;
    case O_LT: v->obj = BOOL_OBJ; v->u.num.val = (v1 < v2); return 0;
    case O_LE: v->obj = BOOL_OBJ; v->u.num.val = (v1 <= v2); return 0;
    case O_GT: v->obj = BOOL_OBJ; v->u.num.val = (v1 > v2); return 0;
    case O_GE: v->obj = BOOL_OBJ; v->u.num.val = (v1 >= v2); return 0;
    case O_ADD: 
        v->obj = SINT_OBJ; 
        v->u.num.val = v1 + v2; return 0;
    case O_SUB: 
        v->obj = (v2 > v1) ? SINT_OBJ : UINT_OBJ; 
        v->u.num.val = v1 - v2; return 0;
    case O_MUL:
        v->obj = UINT_OBJ; 
        v->u.num.val = (uval_t)-v1 * (uval_t)-v2; return 0;
    case O_DIV:
        if ((-v2 == 1) & (v1 == (ival_t)-v1)) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 / v2; return 0;
    case O_MOD:
        v->obj = SINT_OBJ; 
        v->u.num.val = -((uval_t)-v1 % (uval_t)-v2); return 0;
    case O_EXP:
        v->obj = FLOAT_OBJ; 
        v->u.real = pow(v1, v2); 
        if (v->u.real == HUGE_VAL) {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_CONSTNT_LARGE;
            v->u.error.epoint = op->epoint3;
        }
        return 0;
    case O_AND:
        v->obj = SINT_OBJ; 
        v->u.num.val = v1 & v2; return 0;
    case O_OR:
        v->obj = SINT_OBJ; 
        v->u.num.val = v1 | v2; return 0;
    case O_XOR:
        v->obj = SINT_OBJ; 
        v->u.num.val = v1 ^ v2; return 0;
    case O_LSHIFT:
        if ((uval_t)-v2 > 8 * sizeof(v1)) {
            v->obj = UINT_OBJ; 
            v->u.num.val = 0; return 0;
        }
        v->obj = SINT_OBJ; 
        v->u.num.val = v1 >> -v2; return 0;
    case O_RSHIFT:
        if ((uval_t)-v2 > 8 * sizeof(v1)) {
            v->obj = UINT_OBJ; 
            v->u.num.val = 0; return 0;
        }
        v->obj = SINT_OBJ; 
        v->u.num.val = v1 << -v2; return 0;
    default: break;
    }
    return 1;
}

static uint8_t val_len(uval_t val) {
    int span, bits;

    span = 4 * sizeof(uval_t); bits = 0;
    if ((ival_t)val < 0) val = ~val;
    while (span) {
        if (val >> (bits + span)) {
            bits |= span;
        }
        span >>= 1;
    }
    return bits + 2;
}

static void calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_NUM: 
        switch (op->op->u.oper.op) {
        case O_AND:
        case O_OR:
        case O_XOR: if (calc2_num_num(op, op->v1->u.num.val, val_len(op->v1->u.num.val), op->v2->u.num.val, op->v2->u.num.len)) break; return;
        default: break;
        }
        /* fall through */
    case T_BOOL:
    case T_UINT:
        if (calc2_sint_uint(op, op->v1->u.num.val, op->v2->u.num.val)) break; return;
    case T_SINT: 
        if (calc2_sint_sint(op, op->v1->u.num.val, op->v2->u.num.val)) break; return;
    default: op->v2->obj->rcalc2(op); return;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    if (op->op == &o_X) {
        op->v1->obj->repeat(op, (op->v2->u.num.val > 0) ? op->v2->u.num.val : 0); return;
    }
    switch (op->v1->obj->type) {
    case T_NUM: 
        switch (op->op->u.oper.op) {
        case O_LSHIFT:
        case O_RSHIFT:
        case O_AND:
        case O_OR:
        case O_XOR: if (calc2_num_num(op, op->v1->u.num.val, op->v1->u.num.len, op->v2->u.num.val, val_len(op->v2->u.num.val))) break; return;
        default: break;
        }
        /* fall through */
    case T_BOOL:
    case T_UINT:
        if (calc2_uint_sint(op, op->v1->u.num.val, op->v2->u.num.val)) break; return;
    case T_SINT: 
        if (calc2_sint_sint(op, op->v1->u.num.val, op->v2->u.num.val)) break; return;
    default:
        if (op->op != &o_IN) {
            op->v1->obj->calc2(op); return;
        }
    }
    obj_oper_error(op);
}

static int print(const struct value_s *v1, FILE *f) {
    return fprintf(f, "%+" PRIdval, (ival_t)v1->u.num.val);
}

void sintobj_init(void) {
    obj_init(&obj, T_SINT, "<sint>");
    obj.copy = copy;
    obj.copy_temp = copy;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.convert = convert;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.print = print;
}
