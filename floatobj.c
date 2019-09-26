/*
    $Id: floatobj.c 1984 2019-09-20 19:51:05Z soci $

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

static Type obj;

Type *const FLOAT_OBJ = &obj;

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
    case T_ADDRESS: return float_from_address((Address *)v1, epoint);
    default: break;
    }
    return (Obj *)new_error_conv(v1, FLOAT_OBJ, epoint);
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Float *v1 = (const Float *)o1, *v2 = (const Float *)o2;
    return o2->obj == FLOAT_OBJ && v1->real == v2->real;
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types type, linepos_t epoint) {
    Float *v1 = (Float *)o1;
    if (diagnostics.strict_bool && type != TRUTH_BOOL) err_msg_bool(ERROR_____CANT_BOOL, o1, epoint);
    return truth_reference(v1->real != 0.0);
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Float *v1 = (Float *)o1;
    double integer, r;
    int expo;
    unsigned int h;
    r = v1->real;

    if (modf(r, &integer) == 0.0) {
        *hs = ((unsigned int)integer) & ((~0U) >> 1);
        return NULL;
    }
    r = frexp(r, &expo);
    r *= 2147483648.0;
    h = (unsigned int)r;
    r = (r - (double)h) * 2147483648.0;
    h ^= (unsigned int)r ^ ((unsigned int)expo << 15);
    *hs = h & ((~0U) >> 1);
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Float *v1 = (Float *)o1;
    Str *v;
    char line[100];
    int i = 0;
    size_t len = (size_t)sprintf(line, "%.10g", v1->real);
    while (line[i] != 0 && line[i]!='.' && line[i]!='e' && line[i]!='n' && line[i]!='i') i++;
    if (line[i] == 0) {line[i++] = '.';line[i++] = '0';len += 2;}
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    memcpy(v->data, line, len);
    return &v->v;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    double real = floor(((Float *)o1)->real);
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
    if (diagnostics.float_round && real != ((Float *)o1)->real) err_msg2(ERROR___FLOAT_ROUND, NULL, epoint);
    return NULL;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    double real = floor(((Float *)o1)->real);
    Error *v;
    if (real <= -1.0 || real >= (double)(~(uval_t)0) + 1.0) {
        v = new_error(real < 0.0 ? ERROR______NOT_UVAL : ERROR_____CANT_UVAL, epoint);
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
    if (diagnostics.float_round && real != ((Float *)o1)->real) err_msg2(ERROR___FLOAT_ROUND, NULL, epoint);
    return NULL;
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Float *v1 = (Float *)o1;
    if (v1->real < 0.0) return (Obj *)ref_int(minus1_value);
    return (Obj *)ref_int(int_value[(v1->real > 0.0) ? 1 : 0]);
}

static MUST_CHECK Obj *function(Obj *o1, Func_types f, bool inplace, linepos_t UNUSED(epoint)) {
    double r = ((Float *)o1)->real;
    switch (f) {
    case TF_ABS: if (r >= 0.0) return val_reference(o1); r = -r; break;
    case TF_TRUNC: r = trunc(r); break;
    case TF_ROUND: r = round(r); break;
    case TF_FLOOR: r = floor(r); break;
    case TF_CEIL: r = ceil(r); break;
    default: break;
    }
    if (inplace) {
        ((Float *)o1)->real = r;
        return val_reference(o1);
    } 
    return (Obj *)new_float(r);
}

static MUST_CHECK Obj *float_from_double_inplace(double d, oper_t op) {
    if (d == HUGE_VAL || d == -HUGE_VAL) {
        return (Obj *)new_error(ERROR_NUMERIC_OVERF, op->epoint3);
    }
    if (op->inplace == op->v1) {
        ((Float *)op->v1)->real = d;
        return val_reference(op->v1);
    } 
    if (op->inplace == op->v2) {
        ((Float *)op->v2)->real = d;
        return val_reference(op->v2);
    } 
    return (Obj *)new_float(d);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Float *v1 = (Float *)op->v1;
    double real = v1->real;
    switch (op->op->op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
        real = floor(real);
        if (diagnostics.float_round && real != v1->real) err_msg2(ERROR___FLOAT_ROUND, NULL, op->epoint3);
        return bytes_calc1(op->op->op, (unsigned int)((ival_t)real));
    case O_INV: 
        return float_from_double_inplace(-0.5 / ((double)((uint32_t)1 << (8 * sizeof(uint32_t) - 1))) - real, op);
    case O_NEG: 
        return float_from_double_inplace(-real, op);
    case O_POS:
        return val_reference(op->v1);
    case O_STRING:
        {
            Obj *o = repr(op->v1, op->epoint, SIZE_MAX);
            return (o != NULL) ? o : (Obj *)new_error_mem(op->epoint3);
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
            if (op->epoint3->line != 0) err_msg2(ERROR_FLOAT_COMPARE, op->op->name, op->epoint3);
        }
        return true;
    }
    return false;
}

static MUST_CHECK Obj *calc2_double(oper_t op) {
    double r, v1 = ((Float *)op->v1)->real, v2 = ((Float *)op->v2)->real;
    switch (op->op->op) {
    case O_CMP:
        if (v1 == v2 || almost_equal(op, v1, v2)) return (Obj *)ref_int(int_value[0]);
        return (Obj *)ref_int((v1 < v2) ? minus1_value : int_value[1]);
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
            return (Obj *)new_error(ERROR_DIVISION_BY_Z, op->epoint3);
        }
        return float_from_double_inplace(v1 / v2, op);
    case O_MOD:
        if (v2 == 0.0) {
            return (Obj *)new_error(ERROR_DIVISION_BY_Z, op->epoint3);
        }
        r = fmod(v1, v2);
        if (r != 0.0 && ((v2 < 0.0) != (r < 0))) r += v2;
        return float_from_double_inplace(r, op);
    case O_AND: return float_from_double_inplace((double)((uint64_t)(v1 * 4294967296.0) & (uint64_t)(v2 * 4294967296.0)) / 4294967296.0, op);
    case O_OR: return float_from_double_inplace((double)((uint64_t)(v1 * 4294967296.0) | (uint64_t)(v2 * 4294967296.0)) / 4294967296.0, op);
    case O_XOR: return float_from_double_inplace((double)((uint64_t)(v1 * 4294967296.0) ^ (uint64_t)(v2 * 4294967296.0)) / 4294967296.0, op);
    case O_LSHIFT: return float_from_double_inplace(v1 * pow(2.0, v2), op);
    case O_RSHIFT: return float_from_double_inplace(v1 * pow(2.0, -v2), op);
    case O_EXP:
        if (v1 == 0.0) {
            if (v2 < 0.0) {
                return (Obj *)new_error(ERROR_DIVISION_BY_Z, op->epoint3);
            }
            if (v2 != 0.0) {
                return (Obj *)new_float(0.0);
            }
            return (Obj *)new_float(1.0);
        }
        if (v1 < 0.0 && floor(v2) != v2) {
            return (Obj *)new_error(ERROR_NEGFRAC_POWER, op->epoint3);
        }
        return float_from_double_inplace(pow(v1, v2), op);
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
    Obj *err, *val;
    if (op->op == &o_LAND) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference((((Float *)(op->v1))->real != 0.0) ? op->v2 : op->v1);
    }
    if (op->op == &o_LOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference((((Float *)(op->v1))->real != 0.0) ? op->v1 : op->v2);
    }
    switch (op->v2->obj->type) {
    case T_FLOAT: return calc2_double(op);
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        /* fall through */
    case T_INT:
    case T_BITS:
    case T_STR:
    case T_BYTES:
        if (op->op == &o_LSHIFT || op->op == &o_RSHIFT) {
            ival_t shift;
            err = (Obj *)op->v2->obj->ival(op->v2, &shift, 8 * sizeof shift, op->epoint2);
            if (err != NULL) return err;
            if (shift == 0) return val_reference(op->v1);
            if (op->op == &o_RSHIFT) shift = -shift;
            return float_from_double_inplace(ldexp(((Float *)op->v1)->real, shift), op);
        }
        err = create(op->v2, op->epoint2);
        if (err->obj != FLOAT_OBJ) return err;
        op->v2 = err;
        op->inplace = (err->refcount == 1) ? err : NULL;
        val = calc2_double(op);
        val_destroy(err);
        return val;
    default:
        if (op->op != &o_MEMBER && op->op != &o_X) {
            return op->v2->obj->rcalc2(op);
        }
        if (op->v2 == &none_value->v || op->v2->obj == ERROR_OBJ) return val_reference(op->v2);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *err, *val;
    switch (op->v1->obj->type) {
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        /* fall through */
    case T_INT:
    case T_BITS:
        err = create(op->v1, op->epoint);
        if (err->obj != FLOAT_OBJ) return err;
        op->v1 = err;
        op->inplace = (err->refcount == 1) ? err : NULL;
        val = calc2_double(op);
        val_destroy(err);
        return val;
    default: break;
    }
    return obj_oper_error(op);
}

void floatobj_init(void) {
    new_type(&obj, T_FLOAT, "float", sizeof(Float));
    obj.create = create;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.repr = repr;
    obj.ival = ival;
    obj.uval = uval;
    obj.sign = sign;
    obj.function = function;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}

void floatobj_names(void) {
    new_builtin("float", val_reference(&FLOAT_OBJ->v));
    new_builtin("pi", (Obj *)new_float(M_PI));
}
