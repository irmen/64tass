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
#include "floatobj.h"
#include "error.h"
#include "eval.h"
#include "variables.h"

#include "boolobj.h"
#include "codeobj.h"
#include "strobj.h"
#include "boolobj.h"
#include "bytesobj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"

static Type obj;

Type *FLOAT_OBJ = &obj;

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_FLOAT: return val_reference(v1);
    case T_CODE: return float_from_code((Code *)v1, epoint);
    case T_STR: return float_from_str((Str *)v1, epoint);
    case T_BOOL: return (Obj *)float_from_bool((Bool *)v1);
    case T_BYTES: return float_from_bytes((Bytes *)v1, epoint);
    case T_INT: return float_from_int((Int *)v1, epoint);
    case T_BITS: return float_from_bits((Bits *)v1, epoint);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return (Obj *)ref_none();
}

static int same(const Obj *o1, const Obj *o2) {
    const Float *v1 = (const Float *)o1, *v2 = (const Float *)o2;
    return o2->obj == FLOAT_OBJ && v1->real == v2->real;
}

static MUST_CHECK Obj *truth(Obj *o1, enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    Float *v1 = (Float *)o1;
    return truth_reference(!!v1->real);
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Float *v1 = (Float *)o1;
    double integer, r;
    int expo;
    unsigned int h;
    r = v1->real;

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

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Float *v1 = (Float *)o1;
    Str *v;
    char line[100]; 
    int i = 0;
    size_t len;
    len = sprintf(line, "%.10g", v1->real);
    while (line[i] && line[i]!='.' && line[i]!='e' && line[i]!='n' && line[i]!='i') i++;
    if (!line[i]) {line[i++]='.';line[i++]='0';len += 2;}
    if (len > maxsize) return NULL;
    v = new_str(len);
    v->chars = len;
    memcpy(v->data, line, len);
    return &v->v;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, int bits, linepos_t epoint) {
    Float *v1 = (Float *)o1;
    Error *v;
    if (-v1->real >= (double)(~((~(uval_t)0) >> 1)) + 1.0 || v1->real >= (double)((~(uval_t)0) >> 1) + 1.0) {
        *iv = 0;
        v = new_error(ERROR_____CANT_IVAL, epoint);
        v->u.intconv.bits = bits;
        v->u.intconv.val = val_reference(o1);
        return v;
    }
    *iv = v1->real;
    if (((*iv >= 0) ? *iv : (~*iv)) >> (bits-1)) {
        v = new_error(ERROR_____CANT_IVAL, epoint);
        v->u.intconv.bits = bits;
        v->u.intconv.val = val_reference(o1);
        return v;
    }
    return NULL;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, int bits, linepos_t epoint) {
    Float *v1 = (Float *)o1;
    Error *v;
    if (v1->real <= -1.0 || v1->real >= (double)(~(uval_t)0) + 1.0) {
        v = new_error(ERROR_____CANT_UVAL, epoint);
        v->u.intconv.bits = bits;
        v->u.intconv.val = val_reference(o1);
        return v;
    }
    *uv = v1->real;
    if (bits < 8*(int)sizeof(uval_t) && *uv >> bits) {
        v = new_error(ERROR_____CANT_UVAL, epoint);
        v->u.intconv.bits = bits;
        v->u.intconv.val = val_reference(o1);
        return v;
    }
    return NULL;
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Float *v1 = (Float *)o1;
    if (v1->real < 0.0) return (Obj *)ref_int(minus1_value);
    return (Obj *)ref_int(int_value[v1->real > 0.0]);
}

static MUST_CHECK Obj *absolute(Obj *o1, linepos_t UNUSED(epoint)) {
    Float *v1 = (Float *)o1;
    return (v1->real < 0.0) ? (Obj *)new_float(-v1->real) : val_reference(o1);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    double v1 = ((Float *)op->v1)->real;
    ival_t val = v1;
    switch (op->op->op) {
    case O_BANK: val >>= 8;
    case O_HIGHER: val >>= 8;
    case O_LOWER: return (Obj *)bytes_from_u8(val);
    case O_HWORD: val >>= 8;
    case O_WORD: return (Obj *)bytes_from_u16(val);
    case O_BSWORD: return (Obj *)bytes_from_u16((uint8_t)(val >> 8) | (uint16_t)(val << 8));
    case O_INV: return (Obj *)float_from_double(-0.5/((double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1)))-v1, op->epoint);
    case O_NEG: return (Obj *)float_from_double(-v1, op->epoint);
    case O_POS: return val_reference(op->v1);
    case O_STRING: return repr(op->v1, op->epoint, SIZE_MAX);
    case O_LNOT: return truth_reference(!v1);
    default: break;
    }
    return obj_oper_error(op);
}

static int almost_equal(double a, double b) {
    if (a > b) return a - b <= a * 0.0000000005;
    return b - a <= b * 0.0000000005;
}

MUST_CHECK Obj *calc2_double(oper_t op, double v1, double v2) {
    double r;
    switch (op->op->op) {
    case O_CMP: 
        if (almost_equal(v1, v2)) return (Obj *)ref_int(int_value[0]);
        if (v1 < v2) return (Obj *)ref_int(minus1_value);
        return (Obj *)ref_int(int_value[1]);
    case O_EQ: return truth_reference(almost_equal(v1, v2));
    case O_NE: return truth_reference(!almost_equal(v1, v2));
    case O_LT: return truth_reference(v1 < v2 && !almost_equal(v1, v2));
    case O_LE: return truth_reference(v1 < v2 || almost_equal(v1, v2));
    case O_GT: return truth_reference(v1 > v2 && !almost_equal(v1, v2));
    case O_GE: return truth_reference(v1 > v2 || almost_equal(v1, v2));
    case O_ADD: return float_from_double(v1 + v2, op->epoint3);
    case O_SUB: return float_from_double(v1 - v2, op->epoint3);
    case O_MUL: return float_from_double(v1 * v2, op->epoint3);
    case O_DIV:
        if (v2 == 0.0) { 
            return (Obj *)new_error(ERROR_DIVISION_BY_Z, op->epoint3);
        }
        return float_from_double(v1 / v2, op->epoint3);
    case O_MOD:
        if (v2 == 0.0) { 
            return (Obj *)new_error(ERROR_DIVISION_BY_Z, op->epoint3); 
        }
        r = fmod(v1, v2); 
        if (r && ((v2 < 0.0) != (r < 0))) r += v2;
        return (Obj *)new_float(r);
    case O_AND:
        r = (ival_t)floor(v1) & (ival_t)floor(v2);
        v1 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        v2 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        r += ((uint32_t)floor(v1 * 2.0) & (uint32_t)floor(v2 * 2.0))/(double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1)) / 2.0;
        return (Obj *)new_float(r);
    case O_OR:
        r = (ival_t)floor(v1) | (ival_t)floor(v2);
        v1 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        v2 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        r += ((uint32_t)floor(v1 * 2.0) | (uint32_t)floor(v2 * 2.0))/(double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1)) / 2.0;
        return (Obj *)new_float(r);
    case O_XOR:
        r = (ival_t)floor(v1) | (ival_t)floor(v2);
        v1 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        v2 *= (uint32_t)1 << (8 * sizeof(uint32_t) - 1);
        r += ((uint32_t)floor(v1 * 2.0) ^ (uint32_t)floor(v2 * 2.0))/(double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1)) / 2.0;
        return (Obj *)new_float(r);
    case O_LSHIFT: return float_from_double(v1 * pow(2.0, v2), op->epoint3);
    case O_RSHIFT: return (Obj *)new_float(v1 * pow(2.0, -v2));
    case O_EXP: 
        if (!v1) {
            if (v2 < 0.0) {
                return (Obj *)new_error(ERROR_DIVISION_BY_Z, op->epoint3);
            }
            if (v2) {
                return (Obj *)new_float(0.0);
            }
            return (Obj *)new_float(1.0);
        }
        if (v1 < 0.0 && floor(v2) != v2) {
            return (Obj *)new_error(ERROR_NEGFRAC_POWER, op->epoint3);
        }
        return float_from_double(pow(v1, v2), op->epoint3);
    default: break;
    }
    return obj_oper_error(op);
}

MUST_CHECK Obj *float_from_double(double d, linepos_t epoint) {
    if (d == HUGE_VAL || d == -HUGE_VAL) {
        return (Obj *)new_error(ERROR_NUMERIC_OVERF, epoint);
    }
    return (Obj *)new_float(d);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    double d;
    Obj *err;
    if (op->op == &o_LAND) {
        return val_reference(((Float *)(op->v1))->real ? op->v2 : op->v1);
    }
    if (op->op == &o_LOR) {
        return val_reference(((Float *)(op->v1))->real ? op->v1 : op->v2);
    }
    switch (op->v2->obj->type) {
    case T_FLOAT: return calc2_double(op, ((Float *)op->v1)->real, ((Float *)op->v2)->real);
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_STR:
    case T_BYTES:
        err = create(op->v2, op->epoint2);
        if (err->obj != FLOAT_OBJ) return err;
        d = ((Float *)err)->real;
        val_destroy(err);
        return calc2_double(op, ((Float *)op->v1)->real, d);
    default:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return op->v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    double d;
    Obj *err;
    switch (op->v1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
        err = create(op->v1, op->epoint);
        if (err->obj != FLOAT_OBJ) return err;
        d = ((Float *)err)->real;
        val_destroy(err);
        return calc2_double(op, d, ((Float *)op->v2)->real);
    default: break;
    }
    return obj_oper_error(op);
}

void floatobj_init(void) {
    new_type(&obj, T_FLOAT, "float", sizeof(Float));
    obj_init(&obj);
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

void floatobj_names(void) {
    new_builtin("float", val_reference(&FLOAT_OBJ->v));
}
