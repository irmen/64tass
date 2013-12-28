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
#include "boolobj.h"
#include "intobj.h"
#include "addressobj.h"

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

static int MUST_CHECK truth(const struct value_s *v1, struct value_s *UNUSED(v), int *result, enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    *result = !!v1->u.real;
    return 0;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    double integer, r;
    int expo;
    unsigned int h;
    r = v1->u.real;

    if (modf(r, &integer) == 0.0) {
        return ((unsigned int)integer) & ((~(unsigned int)0) >> 1);
    }
    r = frexp(r, &expo);
    r *= 2147483648.0; 
    h = r; 
    r = (r - (double)h) * 2147483648.0;
    h ^= (int)r ^ (expo << 15);
    return h & ((~(unsigned int)0) >> 1);
}

static void repr(const struct value_s *v1, struct value_s *v) {
    char line[100]; 
    int i = 0;
    uint8_t *s;
    sprintf(line, "%.10g", v1->u.real);
    while (line[i] && line[i]!='.' && line[i]!='e' && line[i]!='n' && line[i]!='i') i++;
    if (!line[i]) {line[i++]='.';line[i++]='0';line[i]=0;}
    v->obj = STR_OBJ;
    v->u.str.len = i + strlen(line + i);
    v->u.str.chars = v->u.str.len;
    s = (uint8_t *)malloc(v->u.str.len);
    if (!s) err_msg_out_of_memory();
    memcpy(s, line, v->u.str.len);
    v->u.str.data = s;
    return;
}

static int MUST_CHECK ival(const struct value_s *v1, struct value_s *v, ival_t *iv, int bits, linepos_t epoint) {
    if (-v1->u.real >= (double)(~((~(uval_t)0) >> 1)) + 1.0 || v1->u.real >= (double)((~(uval_t)0) >> 1) + 1.0) {
        *iv = 0;
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_IVAL;
        v->u.error.u.bits = bits;
        v->u.error.epoint = *epoint;
        return 1;
    }
    *iv = v1->u.real;
    if (((*iv >= 0) ? *iv : (~*iv)) >> (bits-1)) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_IVAL;
        v->u.error.u.bits = bits;
        v->u.error.epoint = *epoint;
        return 1;
    }
    return 0;
}

static int MUST_CHECK uval(const struct value_s *v1, struct value_s *v, uval_t *uv, int bits, linepos_t epoint) {
    if (v1->u.real <= -1.0 || v1->u.real >= (double)(~(uval_t)0) + 1.0) {
        *uv = 0;
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_UVAL;
        v->u.error.u.bits = bits;
        v->u.error.epoint = *epoint;
        return 1;
    }
    *uv = v1->u.real;
    if (bits < 8*(int)sizeof(uval_t) && *uv >> bits) {
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_UVAL;
        v->u.error.u.bits = bits;
        v->u.error.epoint = *epoint;
        return 1;
    }
    return 0;
}

static int MUST_CHECK real(const struct value_s *v1, struct value_s *UNUSED(v), double *r, linepos_t UNUSED(epoint)) {
    *r = v1->u.real;
    return 0;
}

static int MUST_CHECK sign(const struct value_s *v1, struct value_s *UNUSED(v), int *s, linepos_t UNUSED(epoint)) {
    *s = (v1->u.real > 0.0) - (v1->u.real < 0.0);
    return 0;
}

static void absolute(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    if (v != v1) copy(v1, v);
    if (v->u.real < 0.0) v->u.real = -v->u.real;
}

static void integer(const struct value_s *v1, struct value_s *v, linepos_t epoint) {
    return int_from_double(v, v1->u.real, epoint);
}

static void calc1(oper_t op) {
    struct value_s *v = op->v;
    double v1 = op->v1->u.real;
    ival_t val = v1;
    switch (op->op->u.oper.op) {
    case O_BANK: val >>= 8;
    case O_HIGHER: val >>= 8;
    case O_LOWER:
        bits_from_u8(v, val);
        return;
    case O_HWORD: val >>= 8;
    case O_WORD:
        bits_from_u16(v, val);
        return;
    case O_BSWORD:
        bits_from_u16(v, (uint8_t)(val >> 8) | (uint16_t)(val << 8));
        return;
    case O_INV: float_from_double(v, -0.5/((double)((uval_t)1 << (8 * sizeof(uval_t) - 1)))-v1); return;
    case O_NEG: float_from_double(v, -v1); return;
    case O_POS: float_from_double(v, v1); return;
    case O_STRING: repr(op->v1, v); return;
    default: break;
    }
    obj_oper_error(op);
}

static int almost_equal(double a, double b) {
    if (a > b) return a - b <= a * 0.0000000005;
    return b - a <= b * 0.0000000005;
}

int calc2_double(oper_t op, double v1, double v2) {
    struct value_s *v = op->v;
    switch (op->op->u.oper.op) {
    case O_CMP: int_from_int(v, almost_equal(v1, v2) ? 0 : ((v1 > v2) - (v1 < v2))); return 0;
    case O_EQ: bool_from_int(v, almost_equal(v1, v2)); return 0;
    case O_NE: bool_from_int(v, !almost_equal(v1, v2)); return 0;
    case O_LT: bool_from_int(v, (v1 < v2) && !almost_equal(v1, v2)); return 0;
    case O_LE: bool_from_int(v, (v1 < v2) || almost_equal(v1, v2)); return 0;
    case O_GT: bool_from_int(v, (v1 > v2) && !almost_equal(v1, v2)); return 0;
    case O_GE: bool_from_int(v, (v1 > v2) || almost_equal(v1, v2)); return 0;
    case O_ADD: float_from_double(v, v1 + v2); return 0;
    case O_SUB: float_from_double(v, v1 - v2); return 0;
    case O_MUL: float_from_double(v, v1 * v2); return 0;
    case O_DIV:
        if (v2 == 0.0) { v->obj = ERROR_OBJ; v->u.error.num = ERROR_DIVISION_BY_Z; v->u.error.epoint = op->epoint2; return 0; }
        float_from_double(v, v1 / v2); return 0;
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
    case O_LSHIFT: float_from_double(v, v1 * pow(2.0, v2)); return 0;
    case O_RSHIFT: float_from_double(v, v1 * pow(2.0, -v2)); return 0;
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

void float_from_double(struct value_s *v, double d) {
    v->obj = FLOAT_OBJ;
    v->u.real = d;
}

static void calc2(oper_t op) {
    double d;
    struct value_s tmp;
    switch (op->v2->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE: 
        if (op->v2->obj->real(op->v2, &tmp, &d, &op->epoint2)) {
            if (op->v1 == op->v || op->v2 == op->v) op->v->obj->destroy(op->v);
            tmp.obj->copy_temp(&tmp, op->v);
            return;
        }
        if (calc2_double(op, op->v1->u.real, d)) break; return;
    default:
        if (op->op != &o_MEMBER) {
            op->v2->obj->rcalc2(op); return;
        }
    }
    obj_oper_error(op);
}

static void rcalc2(oper_t op) {
    double d;
    struct value_s tmp;
    switch (op->v1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
        if (op->v1->obj->real(op->v1, &tmp, &d, &op->epoint)) {
            if (op->v1 == op->v || op->v2 == op->v) op->v->obj->destroy(op->v);
            tmp.obj->copy_temp(&tmp, op->v);
            return;
        }
        if (calc2_double(op, d, op->v2->u.real)) break; return;
    default:
        if (op->op != &o_IN) {
            op->v1->obj->calc2(op); return;
        }
    }
    obj_oper_error(op);
}

void floatobj_init(void) {
    obj_init(&obj, T_FLOAT, "<float>");
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
