/*
    $Id: floatobj.c 3121 2023-09-16 06:38:33Z soci $

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
#include "floatobj.h"
#include <string.h>
#include "math.h"
#include "error.h"
#include "eval.h"
#include "variables.h"
#include "arguments.h"

#include "boolobj.h"
#include "codeobj.h"
#include "strobj.h"
#include "bytesobj.h"
#include "intobj.h"
#include "bitsobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "addressobj.h"
#include "functionobj.h"

static Type obj;

Type *const FLOAT_OBJ = &obj;

MUST_CHECK Obj *float_from_obj(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_FLOAT: return val_reference(v1);
    case T_CODE: return float_from_code(Code(v1), epoint);
    case T_STR: return float_from_str(Str(v1), epoint);
    case T_BOOL: return float_from_bool(Bool(v1));
    case T_BYTES: return float_from_bytes(Bytes(v1), epoint);
    case T_INT: return float_from_int(Int(v1), epoint);
    case T_BITS: return float_from_bits(Bits(v1), epoint);
    case T_ADDRESS: return float_from_address(Address(v1), epoint);
    default: break;
    }
    return new_error_conv(v1, FLOAT_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return float_from_obj(op->v2, op->epoint2);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    return o1->obj == o2->obj && Float(o1)->real == Float(o2)->real;
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types UNUSED(type), linepos_t UNUSED(epoint)) {
    return truth_reference(Float(o1)->real != 0.0);
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    double integer, r = Float(o1)->real;
    int expo;
    unsigned int h, h1, h2;
    bool neg = (r < 0.0);
    if (neg) r = -r;

    r = modf(frexp(r, &expo) * 2147483648.0, &integer) * 2147483648.0;
    h1 = (unsigned int)floor(integer);
    h2 = (unsigned int)floor(r);
    if (neg) {
        h1 = ~h1 + 1U;
        h2 = ~h2 + 1U;
    }
    h = (unsigned int)expo;
    h ^= h1 ^ h2;
    *hs = h & ((~0U) >> 1);
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Str *v;
    char line[100];
    int i = 0;
    size_t len = (size_t)sprintf(line, "%.10g", Float(o1)->real);
    while (line[i] != 0 && line[i]!='.' && line[i]!='e') i++;
    if (line[i] == 0) {line[i++] = '.';line[i++] = '0';len += 2;}
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    memcpy(v->data, line, len);
    return Obj(v);
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    double real = floor(Float(o1)->real);
    Error *v;
    if (-real >= (double)(~((~(uval_t)0) >> 1)) + 1.0 || real >= (double)((~(uval_t)0) >> 1) + 1.0) {
        *iv = 0;
        v = new_error(ERROR_____CANT_IVAL, epoint);
        v->u.intconv.bits = bits;
        v->u.intconv.val = val_reference(o1);
        return v;
    }
    *iv = (ival_t)real;
    if ((((*iv >= 0) ? *iv : (~*iv)) >> (bits - 1)) != 0) {
        v = new_error(ERROR_____CANT_IVAL, epoint);
        v->u.intconv.bits = bits;
        v->u.intconv.val = val_reference(o1);
        return v;
    }
    if (diagnostics.float_round && real != Float(o1)->real) err_msg2(ERROR___FLOAT_ROUND, NULL, epoint);
    return NULL;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    double real = floor(Float(o1)->real);
    Error *v;
    if (real <= -1.0 || real >= (double)(~(uval_t)0) + 1.0) {
        if (real < 0.0) return Error(new_error_obj(ERROR______NOT_UVAL, o1, epoint));
        v = new_error(ERROR_____CANT_UVAL, epoint);
        v->u.intconv.bits = bits;
        v->u.intconv.val = val_reference(o1);
        return v;
    }
    *uv = (uval_t)real;
    if (bits < 8 * sizeof *uv && (*uv >> bits) != 0) {
        v = new_error(ERROR_____CANT_UVAL, epoint);
        v->u.intconv.bits = bits;
        v->u.intconv.val = val_reference(o1);
        return v;
    }
    if (diagnostics.float_round && real != Float(o1)->real) err_msg2(ERROR___FLOAT_ROUND, NULL, epoint);
    return NULL;
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    double v1 = Float(o1)->real;
    return val_reference(v1 < 0.0 ? minus1_value : int_value[(v1 > 0.0) ? 1 : 0]);
}

static MUST_CHECK Obj *function(oper_t op) {
    Float *v1 = Float(op->v2);
    double r = v1->real;
    switch (Function(op->v1)->func) {
    case F_ABS: if (r >= 0.0) return val_reference(Obj(v1)); r = -r; break;
    case F_TRUNC: r = trunc(r); break;
    case F_ROUND: r = round(r); break;
    case F_FLOOR: r = floor(r); break;
    case F_CEIL: r = ceil(r); break;
    default: break;
    }
    if (op->inplace == Obj(v1)) {
        v1->real = r;
        return val_reference(Obj(v1));
    }
    return new_float(r);
}

static MUST_CHECK Obj *float_from_double_inplace(double d, oper_t op) {
    if (d == HUGE_VAL || d == -HUGE_VAL || d != d) {
        return Obj(new_error(ERROR_NUMERIC_OVERF, op->epoint3));
    }
    if (op->inplace == op->v1) {
        Float(op->v1)->real = d;
        return val_reference(op->v1);
    }
    if (op->inplace == op->v2) {
        Float(op->v2)->real = d;
        return val_reference(op->v2);
    }
    return new_float(d);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Float *v1 = Float(op->v1);
    double real = v1->real;
    switch (op->op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
        {
            uint32_t r;
            bool neg = (real < 0.0);
            real = floor(real);
            if (diagnostics.float_round && real != v1->real) err_msg2(ERROR___FLOAT_ROUND, NULL, op->epoint3);
            if (neg) real = -real;
            if (real >= 4294967296.0) real = fmod(real, 4294967296.0);
            r = (uint32_t)real;
            return bits_calc1(op->op, neg ? ~r + 1U : r);
        }
    case O_INV:
        return float_from_double_inplace(-0.5 / ((double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1))) - real, op);
    case O_NEG:
        return float_from_double_inplace(-real, op);
    case O_POS:
        return val_reference(op->v1);
    case O_STRING:
        {
            Obj *o = repr(op->v1, op->epoint, SIZE_MAX);
            return (o != NULL) ? o : new_error_mem(op->epoint3);
        }
    case O_LNOT:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return truth_reference(real == 0.0);
    default: break;
    }
    return obj_oper_error(op);
}

static bool almost_equal(oper_t op, double a, double b) {
    double aa = fabs(a);
    double ab = fabs(b);
    if (fabs(a - b) <= (aa > ab ? ab : aa) * 0.0000000005) {
        if (diagnostics.float_compare) {
            if (op->epoint3->line != 0) err_msg2(ERROR_FLOAT_COMPARE, operators[op->op].name, op->epoint3);
        }
        return true;
    }
    return false;
}

static MUST_CHECK Obj *bitoper(oper_t op) {
    uint64_t v, v1, v2;
    bool neg, neg1, neg2;
    int e, e1, e2;
    double r, r1, r2;

    r1 = frexp(Float(op->v1)->real, &e1);
    neg1 = (r1 < 0.0);
    if (neg1) r1 = -r1;

    r2 = frexp(Float(op->v2)->real, &e2);
    neg2 = (r2 < 0.0);
    if (neg2) r2 = -r2;

    if (e1 > e2) {
        e = e1 - 63;
        e2 = 63 - (e1 - e2);
        e1 = 63;
    } else {
        e = e2 - 63;
        e1 = 63 - (e2 - e1);
        e2 = 63;
    }
    v1 = (uint64_t)ldexp(r1, e1);
    if (neg1 && v1 == 0) v1 = 1;
    v2 = (uint64_t)ldexp(r2, e2);
    if (neg2 && v2 == 0) v2 = 1;

    switch (op->op) {
    case O_AND:
        if (neg1) {
            if (neg2) {
                v = ~((~v1 + 1U) & (~v2 + 1U));
            } else {
                v = (~v1 + 1U) & v2;
            }
        } else if (neg2) {
            v = v1 & (~v2 + 1U);
        } else {
            v = v1 & v2;
        }
        neg = neg1 && neg2;
        break;
    case O_OR:
        if (neg1) {
            if (neg2) {
                v = ~((~v1 + 1U) | (~v2 + 1U));
            } else {
                v = ~((~v1 + 1U) | v2);
            }
        } else if (neg2) {
            v = ~(v1 | (~v2 + 1U));
        } else {
            v = v1 | v2;
        }
        neg = neg1 || neg2;
        break;
    default:
        if (neg1) {
            if (neg2) {
                v = (~v1 + 1U) ^ (~v2 + 1U);
            } else {
                v = ~((~v1 + 1U) ^ v2);
            }
        } else if (neg2) {
            v = ~(v1 ^ (~v2 + 1U));
        } else {
            v = v1 ^ v2;
        }
        neg = neg1 != neg2;
        break;
    }
    r = ldexp((double)v, e);
    return float_from_double_inplace(neg ? -r : r, op);
}

static MUST_CHECK Obj *calc2_double(oper_t op) {
    double r, v1 = Float(op->v1)->real, v2 = Float(op->v2)->real;
    switch (op->op) {
    case O_CMP: return val_reference((v1 == v2 || almost_equal(op, v1, v2)) ? int_value[0] : (v1 < v2) ? minus1_value : int_value[1]);
    case O_EQ: return truth_reference(v1 == v2 || almost_equal(op, v1, v2));
    case O_NE: return truth_reference(v1 != v2 && !almost_equal(op, v1, v2));
    case O_MIN: return truth_reference(v1 < v2);
    case O_LT: return truth_reference(v1 < v2 && !almost_equal(op, v1, v2));
    case O_LE: return truth_reference(v1 <= v2 || almost_equal(op, v1, v2));
    case O_MAX: return truth_reference(v1 > v2);
    case O_GT: return truth_reference(v1 > v2 && !almost_equal(op, v1, v2));
    case O_GE: return truth_reference(v1 >= v2 || almost_equal(op, v1, v2));
    case O_ADD: return float_from_double_inplace(v1 + v2, op);
    case O_SUB: return float_from_double_inplace(v1 - v2, op);
    case O_MUL: return float_from_double_inplace(v1 * v2, op);
    case O_DIV:
        if (v2 == 0.0) {
            return new_error_obj(ERROR_DIVISION_BY_Z, op->v2, op->epoint2);
        }
        return float_from_double_inplace(v1 / v2, op);
    case O_MOD:
        if (v2 == 0.0) {
            return new_error_obj(ERROR_DIVISION_BY_Z, op->v2, op->epoint2);
        }
        r = fmod(v1, v2);
        if (r != 0.0 && ((v2 < 0.0) != (r < 0))) r += v2;
        return float_from_double_inplace(r, op);
    case O_AND:
    case O_OR:
    case O_XOR: return bitoper(op);
    case O_LSHIFT: return float_from_double_inplace(v1 * pow(2.0, v2), op);
    case O_RSHIFT: return float_from_double_inplace(v1 * pow(2.0, -v2), op);
    case O_EXP:
        if (v1 == 0.0) {
            if (v2 < 0.0) {
                return new_error_obj(ERROR_ZERO_NEGPOWER, op->v2, op->epoint2);
            }
            return new_float((v2 == 0.0) ? 1.0 : 0.0);
        }
        if (v1 < 0.0 && floor(v2) != v2) {
            return Obj(new_error(ERROR_NEGFRAC_POWER, op->epoint3));
        }
        return float_from_double_inplace(pow(v1, v2), op);
    default: break;
    }
    return obj_oper_error(op);
}

MUST_CHECK Obj *float_from_double(double d, linepos_t epoint) {
    if (d == HUGE_VAL || d == -HUGE_VAL || d != d) {
        return Obj(new_error(ERROR_NUMERIC_OVERF, epoint));
    }
    return new_float(d);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *err, *val;
    Obj *v2 = op->v2;
    if (op->op == O_LAND) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference((Float(op->v1)->real != 0.0) ? v2 : op->v1);
    }
    if (op->op == O_LOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference((Float(op->v1)->real != 0.0) ? op->v1 : v2);
    }
    if (op->op == O_LXOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return calc2_lxor(op, Float(op->v1)->real != 0.0);
    }
    switch (v2->obj->type) {
    case T_FLOAT: return calc2_double(op);
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        FALL_THROUGH; /* fall through */
    case T_INT:
    case T_BITS:
    case T_STR:
    case T_BYTES:
        if (op->op == O_LSHIFT || op->op == O_RSHIFT) {
            ival_t shift;
            err = Obj(v2->obj->ival(v2, &shift, 8 * sizeof shift, op->epoint2));
            if (err != NULL) return err;
            if (shift == 0) return val_reference(op->v1);
            if (op->op == O_RSHIFT) shift = -shift;
            return float_from_double_inplace(ldexp(Float(op->v1)->real, shift), op);
        }
        err = float_from_obj(v2, op->epoint2);
        if (err->obj != FLOAT_OBJ) return err;
        op->v2 = err;
        op->inplace = (err->refcount == 1) ? err : NULL;
        val = calc2_double(op);
        if (val->obj == ERROR_OBJ) error_obj_update(Error(val), err, v2);
        val_destroy(err);
        return val;
    default:
        if (op->op != O_MEMBER && op->op != O_X) {
            return v2->obj->rcalc2(op);
        }
        if (v2 == none_value || v2->obj == ERROR_OBJ) return val_reference(v2);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *err, *val;
    Obj *v1 = op->v1;
    switch (v1->obj->type) {
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        FALL_THROUGH; /* fall through */
    case T_INT:
    case T_BITS:
        err = float_from_obj(v1, op->epoint);
        if (err->obj != FLOAT_OBJ) return err;
        op->v1 = err;
        op->inplace = (err->refcount == 1) ? err : NULL;
        val = calc2_double(op);
        if (val->obj == ERROR_OBJ) error_obj_update(Error(val), err, v1);
        val_destroy(err);
        return val;
    default: break;
    }
    return obj_oper_error(op);
}

static Float pi_value = { { &obj, 2 }, M_PI };

void floatobj_init(void) {
    Type *type = new_type(&obj, T_FLOAT, "float", sizeof(Float));
    type->convert = convert;
    type->same = same;
    type->truth = truth;
    type->hash = hash;
    type->repr = repr;
    type->ival = ival;
    type->uval = uval;
    type->uval2 = uval;
    type->iaddress = ival;
    type->uaddress = uval;
    type->sign = sign;
    type->function = function;
    type->calc1 = calc1;
    type->calc2 = calc2;
    type->rcalc2 = rcalc2;
    pi_value.v.refcount = 2;
}

void floatobj_names(void) {
    new_builtin("float", val_reference(Obj(FLOAT_OBJ)));
    new_builtin("pi", &pi_value.v);
}

void floatobj_destroy(void) {
#ifdef DEBUG
    if (pi_value.v.refcount != 1) fprintf(stderr, "pi %" PRIuSIZE "\n", pi_value.v.refcount - 1);
#endif
}
