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
#include "values.h"
#include "boolobj.h"

#include "sintobj.h"
#include "uintobj.h"

static struct obj_s obj;

obj_t BOOL_OBJ = &obj;

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = BOOL_OBJ;
    v->refcount = 1;
    v->u.num.val = v1->u.num.val;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == BOOL_OBJ && v1->u.num.val == v2->u.num.val;
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

static void calc1(oper_t op) {
    if (!calc1_uint(op, op->v1->u.num.val)) return;
    obj_oper_error(op);
}

static int calc2_bool_bool(oper_t op, unsigned int v1, unsigned int v2) {
    struct value_s *v = op->v;
    switch (op->op->u.oper.op) {
    case O_CMP: v->obj = (v1 < v2) ? SINT_OBJ : UINT_OBJ; v->u.num.val = (int)v1 - (int)v2; return 0;
    case O_EQ: v->obj = BOOL_OBJ; v->u.num.val = (v1 == v2); return 0;
    case O_NE: v->obj = BOOL_OBJ; v->u.num.val = (v1 != v2); return 0;
    case O_LT: v->obj = BOOL_OBJ; v->u.num.val = (v1 < v2); return 0;
    case O_LE: v->obj = BOOL_OBJ; v->u.num.val = (v1 <= v2); return 0;
    case O_GT: v->obj = BOOL_OBJ; v->u.num.val = (v1 > v2); return 0;
    case O_GE: v->obj = BOOL_OBJ; v->u.num.val = (v1 >= v2); return 0;
    case O_ADD: 
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 + v2; return 0;
    case O_SUB: 
        v->obj = (v1 < v2) ? SINT_OBJ : UINT_OBJ;
        v->u.num.val = v1 - v2; return 0;
    case O_MUL:
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 * v2; return 0;
    case O_DIV:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 / v2; return 0;
    case O_MOD:
        if (!v2) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 % v2; return 0;
    case O_EXP:
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 || !v2; return 0;
    case O_AND:
        v->obj = BOOL_OBJ; 
        v->u.num.val = v1 & v2; return 0;
    case O_OR:
        v->obj = BOOL_OBJ; 
        v->u.num.val = v1 | v2; return 0;
    case O_XOR:
        v->obj = BOOL_OBJ; 
        v->u.num.val = v1 ^ v2; return 0;
    case O_LSHIFT:
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 << v2; return 0;
    case O_RSHIFT:
        v->obj = UINT_OBJ; 
        v->u.num.val = v1 >> v2; return 0;
    default: break;
    }
    return 1;
}

static void calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_BOOL: if (calc2_bool_bool(op, op->v1->u.num.val, op->v2->u.num.val)) break; return;
    default: op->v2->obj->rcalc2(op); return;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    if (op->op == &o_X) {
        op->v1->obj->repeat(op, op->v2->u.num.val); return;
    }
    switch (op->v1->obj->type) {
    case T_BOOL: if (calc2_bool_bool(op, op->v1->u.num.val, op->v2->u.num.val)) break; return;
    default:
        if (op->op != &o_IN) {
            op->v1->obj->calc2(op); return;
        }
    }
    obj_oper_error(op);
}

static int print(const struct value_s *v1, FILE *f) {
    putc(v1->u.num.val ? '1' : '0', f);
    return 1;
}

void boolobj_init(void) {
    obj_init(&obj, T_BOOL, "<bool>");
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
