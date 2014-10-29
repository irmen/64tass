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

#include "boolobj.h"

static struct obj_s obj;

obj_t FLOAT_OBJ = &obj;

static void copy(const struct value_s *v1, struct value_s *v) {
    v->obj = FLOAT_OBJ;
    v->u.real = v1->u.real;
}

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == FLOAT_OBJ && v1->u.real == v2->u.real;
}

static MUST_CHECK struct value_s *truth(const struct value_s *v1, enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    return truth_reference(!!v1->u.real);
}

static MUST_CHECK struct value_s *hash(const struct value_s *v1, int *hs, linepos_t UNUSED(epoint)) {
    double integer, r;
    int expo;
    unsigned int h;
    r = v1->u.real;

    if (modf(r, &integer) == 0.0) {
        *hs = ((unsigned int)integer) & ((~(unsigned int)0) >> 1);
        return NULL;
    }
    r = frexp(r, &expo);
    r *= 2147483648.0; 
    h = r; 
    r = (r - (double)h) * 2147483648.0;
    h ^= (int)r ^ (expo << 15);
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

static MUST_CHECK struct value_s *repr(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    struct value_s *v;
    char line[100]; 
    int i = 0;
    uint8_t *s;
    sprintf(line, "%.10g", v1->u.real);
    while (line[i] && line[i]!='.' && line[i]!='e' && line[i]!='n' && line[i]!='i') i++;
    if (!line[i]) {line[i++]='.';line[i++]='0';line[i]=0;}
    v = val_alloc();
    v->obj = STR_OBJ;
    v->u.str.len = i + strlen(line + i);
    v->u.str.chars = v->u.str.len;
    s = str_create_elements(v, v->u.str.len);
    memcpy(s, line, v->u.str.len);
    v->u.str.data = s;
    return v;
}

static MUST_CHECK struct value_s *ival(const struct value_s *v1, ival_t *iv, int bits, linepos_t epoint) {
    struct value_s *v;
    if (-v1->u.real >= (double)(~((~(uval_t)0) >> 1)) + 1.0 || v1->u.real >= (double)((~(uval_t)0) >> 1) + 1.0) {
        *iv = 0;
        v = val_alloc();
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_IVAL;
        v->u.error.u.bits = bits;
        v->u.error.epoint = *epoint;
        return v;
    }
    *iv = v1->u.real;
    if (((*iv >= 0) ? *iv : (~*iv)) >> (bits-1)) {
        v = val_alloc();
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_IVAL;
        v->u.error.u.bits = bits;
        v->u.error.epoint = *epoint;
        return v;
    }
    return NULL;
}

static MUST_CHECK struct value_s *uval(const struct value_s *v1, uval_t *uv, int bits, linepos_t epoint) {
    struct value_s *v;
    if (v1->u.real <= -1.0 || v1->u.real >= (double)(~(uval_t)0) + 1.0) {
        v = val_alloc();
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_UVAL;
        v->u.error.u.bits = bits;
        v->u.error.epoint = *epoint;
        return v;
    }
    *uv = v1->u.real;
    if (bits < 8*(int)sizeof(uval_t) && *uv >> bits) {
        v = val_alloc();
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR_____CANT_UVAL;
        v->u.error.u.bits = bits;
        v->u.error.epoint = *epoint;
        return v;
    }
    return NULL;
}

static MUST_CHECK struct value_s *real(const struct value_s *v1, double *r, linepos_t UNUSED(epoint)) {
    *r = v1->u.real;
    return NULL;
}

static MUST_CHECK struct value_s *sign(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    if (v1->u.real < 0.0) return int_from_int(-1);
    return val_reference(int_value[v1->u.real > 0.0]);
}

static MUST_CHECK struct value_s *absolute(const struct value_s *v1, linepos_t UNUSED(epoint)) {
    return float_from_double((v1->u.real < 0.0) ? -v1->u.real : v1->u.real);
}

static MUST_CHECK struct value_s *integer(const struct value_s *v1, linepos_t epoint) {
    return int_from_double(v1->u.real, epoint);
}

static MUST_CHECK struct value_s *calc1(oper_t op) {
    double v1 = op->v1->u.real;
    ival_t val = v1;
    switch (op->op->u.oper.op) {
    case O_BANK: val >>= 8;
    case O_HIGHER: val >>= 8;
    case O_LOWER: return bits_from_u8(val);
    case O_HWORD: val >>= 8;
    case O_WORD: return bits_from_u16(val);
    case O_BSWORD: return bits_from_u16((uint8_t)(val >> 8) | (uint16_t)(val << 8));
    case O_INV: return float_from_double(-0.5/((double)((uval_t)1 << (8 * sizeof(uval_t) - 1)))-v1);
    case O_NEG: return float_from_double(-v1);
    case O_POS: return val_reference(op->v1);
    case O_STRING: return repr(op->v1, op->epoint);
    default: break;
    }
    return obj_oper_error(op);
}

static int almost_equal(double a, double b) {
    if (a > b) return a - b <= a * 0.0000000005;
    return b - a <= b * 0.0000000005;
}

MUST_CHECK struct value_s *calc2_double(oper_t op, double v1, double v2) {
    struct value_s *v;
    switch (op->op->u.oper.op) {
    case O_CMP: 
        if (almost_equal(v1, v2)) return val_reference(int_value[0]);
        if (v1 < v2) return int_from_int(-1);
        return val_reference(int_value[1]);
    case O_EQ: return truth_reference(almost_equal(v1, v2));
    case O_NE: return truth_reference(!almost_equal(v1, v2));
    case O_LT: return truth_reference(v1 < v2 && !almost_equal(v1, v2));
    case O_LE: return truth_reference(v1 < v2 || almost_equal(v1, v2));
    case O_GT: return truth_reference(v1 > v2 && !almost_equal(v1, v2));
    case O_GE: return truth_reference(v1 > v2 || almost_equal(v1, v2));
    case O_ADD: return float_from_double(v1 + v2);
    case O_SUB: return float_from_double(v1 - v2);
    case O_MUL: return float_from_double(v1 * v2);
    case O_DIV:
        if (v2 == 0.0) { 
            v = val_alloc();
            v->obj = ERROR_OBJ; 
            v->u.error.num = ERROR_DIVISION_BY_Z; 
            v->u.error.epoint = *op->epoint2;
            return v;
        }
        return float_from_double(v1 / v2);
    case O_MOD:
        if (v2 == 0.0) { 
            v = val_alloc();
            v->obj = ERROR_OBJ; 
            v->u.error.num = ERROR_DIVISION_BY_Z; 
            v->u.error.epoint = *op->epoint2; 
            return v; 
        }
        v = val_alloc();
        v->obj = FLOAT_OBJ; 
        v->u.real = fmod(v1, v2); 
        if (v->u.real && ((v2 < 0.0) != (v->u.real < 0))) v->u.real += v2;
        return v;
    case O_AND:
        v = val_alloc();
        v->obj = FLOAT_OBJ; 
        v->u.real = (ival_t)floor(v1) & (ival_t)floor(v2);
        v1 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v2 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v->u.real += ((uval_t)floor(v1 * 2.0) & (uval_t)floor(v2 * 2.0))/(double)((uval_t)1 << (8 * sizeof(uval_t) - 1)) / 2.0;
        return v;
    case O_OR:
        v = val_alloc();
        v->obj = FLOAT_OBJ; 
        v->u.real = (ival_t)floor(v1) | (ival_t)floor(v2);
        v1 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v2 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v->u.real += ((uval_t)floor(v1 * 2.0) | (uval_t)floor(v2 * 2.0))/(double)((uval_t)1 << (8 * sizeof(uval_t) - 1)) / 2.0;
        return v;
    case O_XOR:
        v = val_alloc();
        v->obj = FLOAT_OBJ; 
        v->u.real = (ival_t)floor(v1) | (ival_t)floor(v2);
        v1 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v2 *= (uval_t)1 << (8 * sizeof(uval_t) - 1);
        v->u.real += ((uval_t)floor(v1 * 2.0) ^ (uval_t)floor(v2 * 2.0))/(double)((uval_t)1 << (8 * sizeof(uval_t) - 1)) / 2.0;
        return v;
    case O_LSHIFT: return float_from_double(v1 * pow(2.0, v2));
    case O_RSHIFT: return float_from_double(v1 * pow(2.0, -v2));
    case O_EXP: 
        if (!v1) {
            if (v2 < 0.0) {
                v = val_alloc();
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_DIVISION_BY_Z;
                v->u.error.epoint = *op->epoint2;
                return v;
            }
            v = val_alloc();
            v->obj = FLOAT_OBJ; 
            v->u.real = 0.0;
            return v;
        } 
        if (v1 < 0.0 && floor(v2) != v2) {
            v = val_alloc();
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_NEGFRAC_POWER;
            v->u.error.epoint = *op->epoint2;
            return v;
        }
        v = val_alloc();
        v->obj = FLOAT_OBJ; 
        v->u.real = pow(v1, v2); 
        if (v->u.real == HUGE_VAL) {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_CONSTNT_LARGE;
            v->u.error.epoint = *op->epoint3;
        }
        return v;
    default: break;
    }
    return obj_oper_error(op);
}

MUST_CHECK struct value_s *float_from_double(double d) {
    struct value_s *v = val_alloc();
    v->obj = FLOAT_OBJ;
    v->u.real = d;
    return v;
}

static MUST_CHECK struct value_s *calc2(oper_t op) {
    double d;
    struct value_s *err;
    switch (op->v2->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE: 
        err = op->v2->obj->real(op->v2, &d, op->epoint2);
        if (err) return err;
        return calc2_double(op, op->v1->u.real, d);
    default:
        if (op->op != &o_MEMBER) {
            return op->v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK struct value_s *rcalc2(oper_t op) {
    double d;
    struct value_s *err;
    switch (op->v1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
        err = op->v1->obj->real(op->v1, &d, op->epoint);
        if (err) return err;
        return calc2_double(op, d, op->v2->u.real);
    default:
        if (op->op != &o_IN) {
            return op->v1->obj->calc2(op);
        }
    }
    return obj_oper_error(op);
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
