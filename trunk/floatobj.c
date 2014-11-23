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

static MUST_CHECK value_t create(const value_t v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_FLOAT: return val_reference(v1);
    case T_CODE: return float_from_code(v1, epoint);
    case T_STR: return float_from_str(v1, epoint);
    case T_BOOL: return float_from_bool(v1);
    case T_BYTES: return float_from_bytes(v1, epoint);
    case T_INT: return float_from_int(v1, epoint);
    case T_BITS: return float_from_bits(v1, epoint);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return val_reference(none_value);
}

static int same(const value_t v1, const value_t v2) {
    return v2->obj == FLOAT_OBJ && v1->u.real == v2->u.real;
}

static MUST_CHECK value_t truth(const value_t v1, enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    return truth_reference(!!v1->u.real);
}

static MUST_CHECK value_t hash(const value_t v1, int *hs, linepos_t UNUSED(epoint)) {
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

static MUST_CHECK value_t repr(const value_t v1, linepos_t UNUSED(epoint)) {
    value_t v;
    char line[100]; 
    int i = 0;
    uint8_t *s;
    sprintf(line, "%.10g", v1->u.real);
    while (line[i] && line[i]!='.' && line[i]!='e' && line[i]!='n' && line[i]!='i') i++;
    if (!line[i]) {line[i++]='.';line[i++]='0';line[i]=0;}
    v = val_alloc(STR_OBJ);
    v->u.str.len = i + strlen(line + i);
    v->u.str.chars = v->u.str.len;
    s = str_create_elements(v, v->u.str.len);
    memcpy(s, line, v->u.str.len);
    v->u.str.data = s;
    return v;
}

static MUST_CHECK value_t ival(const value_t v1, ival_t *iv, int bits, linepos_t epoint) {
    value_t v;
    if (-v1->u.real >= (double)(~((~(uval_t)0) >> 1)) + 1.0 || v1->u.real >= (double)((~(uval_t)0) >> 1) + 1.0) {
        *iv = 0;
        v = new_error_obj(ERROR_____CANT_IVAL, epoint);
        v->u.error.u.bits = bits;
        return v;
    }
    *iv = v1->u.real;
    if (((*iv >= 0) ? *iv : (~*iv)) >> (bits-1)) {
        v = new_error_obj(ERROR_____CANT_IVAL, epoint);
        v->u.error.u.bits = bits;
        return v;
    }
    return NULL;
}

static MUST_CHECK value_t uval(const value_t v1, uval_t *uv, int bits, linepos_t epoint) {
    value_t v;
    if (v1->u.real <= -1.0 || v1->u.real >= (double)(~(uval_t)0) + 1.0) {
        v = new_error_obj(ERROR_____CANT_UVAL, epoint);
        v->u.error.u.bits = bits;
        return v;
    }
    *uv = v1->u.real;
    if (bits < 8*(int)sizeof(uval_t) && *uv >> bits) {
        v = new_error_obj(ERROR_____CANT_UVAL, epoint);
        v->u.error.u.bits = bits;
        return v;
    }
    return NULL;
}

static MUST_CHECK value_t sign(const value_t v1, linepos_t UNUSED(epoint)) {
    if (v1->u.real < 0.0) return int_from_int(-1);
    return val_reference(int_value[v1->u.real > 0.0]);
}

static MUST_CHECK value_t absolute(const value_t v1, linepos_t UNUSED(epoint)) {
    return (v1->u.real < 0.0) ? float_from_double(-v1->u.real) : val_reference(v1);
}

static MUST_CHECK value_t calc1(oper_t op) {
    double v1 = op->v1->u.real;
    ival_t val = v1;
    switch (op->op->u.oper.op) {
    case O_BANK: val >>= 8;
    case O_HIGHER: val >>= 8;
    case O_LOWER: return bytes_from_u8(val);
    case O_HWORD: val >>= 8;
    case O_WORD: return bytes_from_u16(val);
    case O_BSWORD: return bytes_from_u16((uint8_t)(val >> 8) | (uint16_t)(val << 8));
    case O_INV: return float_from_double(-0.5/((double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1)))-v1);
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

MUST_CHECK value_t calc2_double(oper_t op, double v1, double v2) {
    double r;
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
    case O_ADD: return float_from_double2(v1 + v2, op->epoint3);
    case O_SUB: return float_from_double2(v1 - v2, op->epoint3);
    case O_MUL: return float_from_double2(v1 * v2, op->epoint3);
    case O_DIV:
        if (v2 == 0.0) { 
            return new_error_obj(ERROR_DIVISION_BY_Z, op->epoint3);
        }
        return float_from_double2(v1 / v2, op->epoint3);
    case O_MOD:
        if (v2 == 0.0) { 
            return new_error_obj(ERROR_DIVISION_BY_Z, op->epoint3); 
        }
        r = fmod(v1, v2); 
        if (r && ((v2 < 0.0) != (r < 0))) r += v2;
        return float_from_double(r);
    case O_AND:
        r = (ival_t)floor(v1) & (ival_t)floor(v2);
        v1 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        v2 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        r += ((uint32_t)floor(v1 * 2.0) & (uint32_t)floor(v2 * 2.0))/(double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1)) / 2.0;
        return float_from_double(r);
    case O_OR:
        r = (ival_t)floor(v1) | (ival_t)floor(v2);
        v1 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        v2 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        r += ((uint32_t)floor(v1 * 2.0) | (uint32_t)floor(v2 * 2.0))/(double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1)) / 2.0;
        return float_from_double(r);
    case O_XOR:
        r = (ival_t)floor(v1) | (ival_t)floor(v2);
        v1 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        v2 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        r += ((uint32_t)floor(v1 * 2.0) ^ (uint32_t)floor(v2 * 2.0))/(double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1)) / 2.0;
        return float_from_double(r);
    case O_LSHIFT: return float_from_double2(v1 * pow(2.0, v2), op->epoint3);
    case O_RSHIFT: return float_from_double(v1 * pow(2.0, -v2));
    case O_EXP: 
        if (!v1) {
            if (v2 < 0.0) {
                return new_error_obj(ERROR_DIVISION_BY_Z, op->epoint3);
            }
            return float_from_double(0.0);
        } 
        if (v1 < 0.0 && floor(v2) != v2) {
            return new_error_obj(ERROR_NEGFRAC_POWER, op->epoint3);
        }
        return float_from_double2(pow(v1, v2), op->epoint3);
    default: break;
    }
    return obj_oper_error(op);
}

MUST_CHECK value_t float_from_double(double d) {
    value_t v = val_alloc(FLOAT_OBJ);
    v->u.real = d;
    return v;
}

MUST_CHECK value_t float_from_double2(double d, linepos_t epoint) {
    if (d == HUGE_VAL || d == -HUGE_VAL) {
        return new_error_obj(ERROR_NUMERIC_OVERF, epoint);
    }
    return float_from_double(d);
}

static MUST_CHECK value_t calc2(oper_t op) {
    double d;
    value_t err;
    switch (op->v2->obj->type) {
    case T_FLOAT: return calc2_double(op, op->v1->u.real, op->v2->u.real);
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_STR:
    case T_BYTES:
        err = create(op->v2, op->epoint2);
        if (err->obj != FLOAT_OBJ) return err;
        d = err->u.real;
        val_destroy(err);
        return calc2_double(op, op->v1->u.real, d);
    default:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return op->v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    double d;
    value_t err;
    switch (op->v1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
        err = create(op->v1, op->epoint);
        if (err->obj != FLOAT_OBJ) return err;
        d = err->u.real;
        val_destroy(err);
        return calc2_double(op, d, op->v2->u.real);
    default: break;
    }
    return obj_oper_error(op);
}

void floatobj_init(void) {
    obj_init(&obj, T_FLOAT, "float");
    obj.create = create;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.repr = repr;
    obj.ival = ival;
    obj.uval = uval;
    obj.sign = sign;
    obj.abs = absolute;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
