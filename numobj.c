/*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#include <stdlib.h>
#include <string.h>
#include "obj.h"
#include "numobj.h"
#include "eval.h"

#include "strobj.h"
#include "addressobj.h"
#include "boolobj.h"
#include "uintobj.h"
#include "sintobj.h"

static struct obj_s obj;

obj_t NUM_OBJ = &obj;

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = NUM_OBJ;
    v->refcount = 1;
    v->u.num.val = v1->u.num.val;
    v->u.num.len = v1->u.num.len;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == NUM_OBJ && v1->u.num.len == v2->u.num.len && v1->u.num.val == v2->u.num.val;
}

static int truth(const struct value_s *v1) {
    return !!v1->u.num.val;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    return v1->u.num.val & ((~(unsigned int)0) >> 1);
}

static void convert(struct value_s *v1, struct value_s *v, obj_t t, linepos_t epoint, linepos_t epoint2) {
    obj.convert = UINT_OBJ->convert;
    obj.convert(v1, v, t, epoint, epoint2);
}

int calc1_num(oper_t op, uval_t v1, uint8_t len) {
    struct value_s *v = op->v;
    enum atype_e am;

    switch (op->op->u.oper.op) {
    case O_BANK: v1 >>= 8;
    case O_HIGHER: v1 >>= 8;
    case O_LOWER:
        v->u.num.val = (uint8_t)v1; 
        len = 8;
        break;
    case O_HWORD: v1 >>= 8;
    case O_WORD:
        v->u.num.val = (uint16_t)v1; 
        len = 16; 
        break;
    case O_BSWORD:
        v->u.num.val = (uint8_t)(v1 >> 8) | (uint16_t)(v1 << 8); 
        len = 16; 
        break;
    case O_COMMAS: am = A_SR; goto addr;
    case O_COMMAR: am = A_RR; goto addr;
    case O_COMMAZ: am = A_ZR; goto addr;
    case O_COMMAY: am = A_YR; goto addr;
    case O_COMMAX: am = A_XR; goto addr;
    case O_HASH: am = A_IMMEDIATE;
    addr:
        v->obj = ADDRESS_OBJ; 
        v->u.addr.val = v1; 
        v->u.addr.len = len;
        v->u.addr.type = am; 
        return 0;
    case O_INV:
        v->u.num.val = ~v1;
        break;
    case O_NEG:
        v->u.num.val = -v1;
        break;
    case O_POS:
        v->u.num.val = v1;
        break;
    case O_LNOT:
        v->obj = BOOL_OBJ; 
        v->u.num.val = !v1;
        return 0;
    case O_STRING:
        convert(op->v1, v, STR_OBJ, op->epoint, op->epoint3);
        return 0;
    default: return 1;
    }
    v->obj = NUM_OBJ;
    v->u.num.len = len;
    return 0;
}

static void calc1(oper_t op) {
    if (!calc1_num(op, op->v1->u.num.val, op->v1->u.num.len)) return;
    obj_oper_error(op);
}

int calc2_num_num(oper_t op, uval_t v1, uint8_t len1, uval_t v2, uint8_t len2) {
    struct value_s *v = op->v;
    switch (op->op->u.oper.op) {
    case O_AND: 
        v->obj = NUM_OBJ; 
        v->u.num.len = (len1 > len2) ? len1 : len2; 
        v->u.num.val = v1 & v2; return 0;
    case O_OR: 
        v->obj = NUM_OBJ; 
        v->u.num.len = (len1 > len2) ? len1 : len2; 
        v->u.num.val = v1 | v2; return 0;
    case O_XOR: 
        v->obj = NUM_OBJ; 
        v->u.num.len = (len1 > len2) ? len1 : len2; 
        v->u.num.val = v1 ^ v2; return 0;
    case O_LSHIFT:
        if (v2 + len1 > 8 * sizeof(v->u.num.val)) {
            v->obj = UINT_OBJ; 
            v->u.num.len = 8 * sizeof(v->u.num.val);
            v->u.num.val = 0; return 0;
        }
        v->obj = NUM_OBJ; 
        v->u.num.len = len1 + v2;
        v->u.num.val = v1 << v2; return 0;
    case O_RSHIFT:
        v->obj = NUM_OBJ; 
        if (v2 > len1) {
            v->u.num.len = 0;
            v->u.num.val = 0; return 0;
        }
        v->u.num.len = len1 - v2;
        v->u.num.val = v1 >> v2; return 0;
    case O_CONCAT: 
        v->obj = NUM_OBJ;
        if (len2 >= 8 * sizeof(v->u.num.val)) {
            v->u.num.val = v2;
            v->u.num.len = v2;
            return 0;
        }
        v1 &= (((uval_t)1 << len1)-1);
        v2 &= (((uval_t)1 << len2)-1);
        v->u.num.val = (v1 << len2) | v2;
        v->u.num.len = ((len1 + len2) > 8 * sizeof(v->u.num.val)) ? 8 * sizeof(v->u.num.val) : (len1 + len2);
        return 0;
    default: return calc2_uint_uint(op, v1, v2);
    }
}

static void calc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2;
    switch (v2->obj->type) {
    case T_NUM: 
        if (calc2_num_num(op, v1->u.num.val, v1->u.num.len, v2->u.num.val, v2->u.num.len)) break; return;
    case T_BOOL:
        if (calc2_num_num(op, v1->u.num.val, v1->u.num.len, v2->u.num.val, 1)) break; return;
    default: v2->obj->rcalc2(op); return;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    struct value_s *v1 = op->v1, *v2 = op->v2;
    if (op->op == &o_X) {
        v1->obj->repeat(op, v2->u.num.val); return;
    }
    switch (v1->obj->type) {
    case T_NUM: 
        if (calc2_num_num(op, v1->u.num.val, v1->u.num.len, v2->u.num.val, v2->u.num.len)) break; return;
    case T_BOOL:
        if (calc2_num_num(op, v1->u.num.val, 1, v2->u.num.val, v2->u.num.val)) break; return;
    default:
        if (op->op != &o_IN) {
            v1->obj->calc2(op); return;
        }
    }
    obj_oper_error(op);
}

static void print(const struct value_s *v1, FILE *f) {
    fprintf(f, "$%" PRIxval, (uval_t)v1->u.num.val);
}

static void iindex(oper_t op) {
    int i, len;
    ival_t offs;
    uval_t val = 0;
    struct value_s *v1 = op->v1, *v2 = op->v2, *v = op->v;

    len = (v2->u.list.len < 8*sizeof(val)) ? v2->u.list.len : 8*sizeof(val);
    for (i = 0; i < len; i++) {
        offs = indexoffs(v2->u.list.data[i], v1->u.num.len);
        if (offs < 0) {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR___INDEX_RANGE;
            v->u.error.epoint = op->epoint2;
            return;
        }
        if (v1->u.num.val & (1 << offs)) val |= 1 << i;
    }
    v->obj = NUM_OBJ;
    v->u.num.len = i;
    v->u.num.val = val;
}

static void slice(struct value_s *v1, ival_t offs, ival_t end, ival_t step, struct value_s *v, linepos_t UNUSED(epoint)) {
    size_t i;
    size_t len;
    uval_t val;

    if (step > 0) {
        if (end < offs) end = offs;
        len = (end - offs + step - 1) / step;
    } else {
        if (end > offs) end = offs;
        len = (offs - end - step - 1) / -step;
    }

    if (step == 1) {
        val = (v1->u.num.val >> offs) & (((uval_t)1 << len)-1);
    } else {
        i = 0; val = 0;
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            if (v1->u.num.val & (1 << offs)) val |= 1 << i;
            i++; offs += step;
        }
    }
    v->obj = NUM_OBJ;
    v->u.num.len = len;
    v->u.num.val = val;
}

void numobj_init(void) {
    obj_init(&obj, T_NUM, "<num>");
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
    obj.iindex = iindex;
    obj.slice = slice;
}
