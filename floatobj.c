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
#include "floatobj.h"

#include "strobj.h"
#include "numobj.h"
#include "boolobj.h"
#include "sintobj.h"
#include "uintobj.h"

static struct obj_s obj;

obj_t FLOAT_OBJ = &obj;

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = FLOAT_OBJ;
    v->refcount = 1;
    v->u.real = v1->u.real;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == FLOAT_OBJ && v1->u.real == v2->u.real;
}

static int truth(const struct value_s *v1) {
    return !!v1->u.real;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    double integer, r;
    int exp;
    unsigned int h;
    r = v1->u.real;

    if (modf(r, &integer) == 0.0) {
        return ((unsigned int)integer) & ((~(unsigned int)0) >> 1);
    }
    r = frexp(r, &exp);
    r *= 2147483648.0; 
    h = r; 
    r = (r - (double)h) * 2147483648.0;
    h ^= (int)r ^ (exp << 15);
    return h & ((~(unsigned int)0) >> 1);
}

static void convert(struct value_s *v1, struct value_s *v, obj_t t, linepos_t UNUSED(epoint), linepos_t UNUSED(epoint2)) {
    if (t == STR_OBJ) {
        char line[100]; 
        int i = 0;
        uint8_t *s;
        sprintf(line, "%.10g", v1->u.real);
        while (line[i] && line[i]!='.' && line[i]!='e' && line[i]!='n' && line[i]!='i') i++;
        if (!line[i]) {line[i++]='.';line[i++]='0';line[i]=0;}
        v->obj = t;
        v->u.str.len = i + strlen(line + i);
        v->u.str.chars = v->u.str.len;
        s = malloc(v->u.str.len);
        if (!s) err_msg_out_of_memory();
        memcpy(s, line, v->u.str.len);
        v->u.str.data = s;
        return;
    }
    /////////////////////////////////////obj_oper_error(O_STRING, v1, NULL, v, epoint);
}

static int calc1_double(oper_t op, double v1) {
    struct value_s *v = op->v;
    switch (op->op->u.oper.op) {
    case O_INV:
        v->obj = FLOAT_OBJ;
        v->u.real = -0.5/((double)((uval_t)1 << (8 * sizeof(uval_t) - 1)))-v1;
        return 0;
    case O_NEG:
        v->obj = FLOAT_OBJ;
        v->u.real = -v1;
        return 0;
    case O_POS:
        v->obj = FLOAT_OBJ;
        v->u.real = v1;
        return 0;
    case O_LNOT:
        v->obj = BOOL_OBJ; 
        v->u.num.val = !v1;
        return 0;
    case O_STRING:
        convert(op->v1, v, STR_OBJ, &op->epoint, &op->epoint3);
        return 0;
    default: return calc1_sint(op, v1);
    }
    return 1;
}

static void calc1(oper_t op) {
    if (!calc1_double(op, op->v1->u.real)) return;
    obj_oper_error(op);
}

static int almost_equal(double a, double b) {
    if (a > b) return a - b < a * 0.0000000005;
    return b - a < b * 0.0000000005;
}

static int calc2_double(oper_t op, double v1, double v2) {
    struct value_s *v = op->v;
    switch (op->op->u.oper.op) {
    case O_CMP: v->u.num.val = almost_equal(v1, v2) ? 0 : ((v1 > v2) - (v1 < v2)); v->obj = (v->u.num.val < 0) ? SINT_OBJ : UINT_OBJ; return 0;
    case O_EQ: v->obj = BOOL_OBJ; v->u.num.val = almost_equal(v1, v2); return 0;
    case O_NE: v->obj = BOOL_OBJ; v->u.num.val = !almost_equal(v1, v2); return 0;
    case O_LT: v->obj = BOOL_OBJ; v->u.num.val = (v1 < v2) && !almost_equal(v1, v2); return 0;
    case O_LE: v->obj = BOOL_OBJ; v->u.num.val = (v1 < v2) || almost_equal(v1, v2); return 0;
    case O_GT: v->obj = BOOL_OBJ; v->u.num.val = (v1 > v2) && !almost_equal(v1, v2); return 0;
    case O_GE: v->obj = BOOL_OBJ; v->u.num.val = (v1 > v2) || almost_equal(v1, v2); return 0;
    case O_ADD: 
        v->obj = FLOAT_OBJ; 
        v->u.real = v1 + v2; return 0;
    case O_SUB: 
        v->obj = FLOAT_OBJ; 
        v->u.real = v1 - v2; return 0;
    case O_MUL:
        v->obj = FLOAT_OBJ; 
        v->u.real = v1 * v2; return 0;
    case O_DIV:
        if (v2 == 0.0) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        v->obj = FLOAT_OBJ; 
        v->u.real = v1 / v2; return 0;
    case O_MOD:
        if (v2 == 0.0) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        v->obj = FLOAT_OBJ; 
        v->u.real = fmod(v1, v2); 
        if (v->u.real && ((v2 < 0.0) != (v->u.real < 0))) v->u.real += v2;
        return 0;
    case O_AND:
        v->obj = FLOAT_OBJ; 
        v->u.real = (ival_t)floor(v1) & (ival_t)floor(v2);
        v1 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v2 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v->u.real += ((uval_t)floor(v1 * 2.0) & (uval_t)floor(v2 * 2.0))/(double)((uval_t)1 << (8 * sizeof(uval_t) - 1)) / 2.0;
        return 0;
    case O_OR:
        v->obj = FLOAT_OBJ; 
        v->u.real = (ival_t)floor(v1) | (ival_t)floor(v2);
        v1 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v2 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v->u.real += ((uval_t)floor(v1 * 2.0) | (uval_t)floor(v2 * 2.0))/(double)((uval_t)1 << (8 * sizeof(uval_t) - 1)) / 2.0;
        return 0;
    case O_XOR:
        v->obj = FLOAT_OBJ; 
        v->u.real = (ival_t)floor(v1) | (ival_t)floor(v2);
        v1 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v2 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v->u.real += ((uval_t)floor(v1 * 2.0) ^ (uval_t)floor(v2 * 2.0))/(double)((uval_t)1 << (8 * sizeof(uval_t) - 1)) / 2.0;
        return 0;
    case O_LSHIFT:
        v->obj = FLOAT_OBJ; 
        v->u.real = v1 * pow(2.0, v2); 
        return 0;
    case O_RSHIFT:
        v->obj = FLOAT_OBJ; 
        v->u.real = v1 * pow(2.0, -v2);
        return 0;
    case O_EXP: 
        if (!v1) {
            if (v2 < 0.0) {
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_DIVISION_BY_Z;
                v->u.error.epoint = op->epoint2;
                return 0;
            }
            v->obj = FLOAT_OBJ; 
            v->u.real = 0.0;
            return 0;
        } 
        if (v1 < 0.0 && floor(v2) != v2) {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_NEGFRAC_POWER;
            v->u.error.epoint = op->epoint2;
            return 0;
        }
        v->obj = FLOAT_OBJ; 
        v->u.real = pow(v1, v2); 
        if (v->u.real == HUGE_VAL) {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_CONSTNT_LARGE;
            v->u.error.epoint = op->epoint3;
        }
        return 0;
    default: break;
    }
    return 1;
}

static void calc2(oper_t op) {
    switch (op->v2->obj->type) {
    case T_NUM: 
        if (op->v1->u.real == floor(op->v1->u.real)) {
            switch (op->op->u.oper.op) {
            case O_AND:
            case O_OR:
            case O_XOR: if (calc2_num_num(op, op->v1->u.real, 0, op->v2->u.num.val, op->v2->u.num.len)) break; return;
            default: break;
            }
        }
        /* fall through */
    case T_BOOL:
    case T_UINT: if (calc2_double(op, op->v1->u.real, (uval_t)op->v2->u.num.val)) break; return;
    case T_SINT: if (calc2_double(op, op->v1->u.real, op->v2->u.num.val)) break; return;
    case T_FLOAT: if (calc2_double(op, op->v1->u.real, op->v2->u.real)) break; return;
    case T_CODE: if (calc2_double(op, op->v1->u.real, op->v2->u.code.addr)) break; return;
    default: op->v2->obj->rcalc2(op); return;
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    if (op->op == &o_X) {
        if (op->v2->u.real == floor(op->v2->u.real)) op->v1->obj->repeat(op, (op->v2->u.real > 0) ? op->v2->u.real : 0);
        else obj_oper_error(op);
        return;
    }
    switch (op->v1->obj->type) {
    case T_NUM: 
        if (op->v2->u.real == floor(op->v2->u.real)) {
            switch (op->op->u.oper.op) {
            case O_LSHIFT:
            case O_RSHIFT:
            case O_AND:
            case O_OR:
            case O_XOR: if (calc2_num_num(op, op->v1->u.num.val, 0, op->v2->u.num.val, op->v2->u.num.len)) break; return;
            default: break;
            }
        }
        /* fall through */
    case T_BOOL:
    case T_UINT: if (calc2_double(op, (uval_t)op->v1->u.num.val, op->v2->u.real)) break; return;
    case T_SINT: if (calc2_double(op, op->v1->u.num.val, op->v2->u.real)) break; return;
    case T_FLOAT: if (calc2_double(op, op->v1->u.real, op->v2->u.real)) break; return;
    case T_CODE: if (calc2_double(op, op->v1->u.code.addr, op->v2->u.real)) break; return;
    default:
        if (op->op != &o_IN) {
            op->v1->obj->calc2(op); return;
        }
    }
    obj_oper_error(op);
}

static int print(const struct value_s *v1, FILE *f) {
    char num[100];
    int i = 0, l;
    l = sprintf(num, "%.10g", v1->u.real);
    while (num[i] && num[i]!='.' && num[i]!='e' && num[i]!='n' && num[i]!='i') i++;
    if (!num[i]) {num[i++]='.';num[i++]='0';num[i]=0;l+=2;}
    fputs(num, f);
    return l;
}

void floatobj_init(void) {
    obj_init(&obj, T_FLOAT, "<float>");
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
