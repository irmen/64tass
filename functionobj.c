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
#include "isnprintf.h"
#include "functionobj.h"
#include "eval.h"
#include "variables.h"
#include "floatobj.h"

static struct obj_s obj;

obj_t FUNCTION_OBJ = &obj;

static int same(const value_t v1, const value_t v2) {
    return v2->obj == FUNCTION_OBJ && v1->u.function.func == v2->u.function.func;
}

static MUST_CHECK value_t hash(const value_t v1, int *hs, linepos_t UNUSED(epoint)) {
    *hs = v1->u.function.name_hash;
    return NULL;
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t epoint) {
    uint8_t *s;
    const char *prefix;
    size_t len;
    value_t v;
    if (!epoint) return NULL;
    prefix = "<native_function '";
    len = strlen(prefix);
    v = val_alloc(STR_OBJ);
    v->u.str.len = v1->u.function.name.len + 2 + len;
    if (v->u.str.len < (2 + len)) err_msg_out_of_memory(); /* overflow */
    v->u.str.chars = v->u.str.len;
    s = str_create_elements(v, v->u.str.len);
    memcpy(s, prefix, len);
    memcpy(s + len, v1->u.function.name.data, v1->u.function.name.len);
    s[v->u.str.len - 2] = '\'';
    s[v->u.str.len - 1] = '>';
    v->u.str.data = s;
    return v;
}

/* range([start],end,[step]) */
static inline MUST_CHECK value_t function_range(value_t vals, linepos_t epoint) {
    struct values_s *v = vals->u.funcargs.val;
    size_t args = vals->u.funcargs.len;
    value_t err, new_value;
    ival_t start = 0, end, step = 1;
    size_t i = 0, len2;
    value_t *val;

    if (args < 1 || args > 3) {
        err_msg_argnum(args, 1, 3, epoint);
        return val_reference(none_value);
    }
    switch (args) {
    case 1: 
        err = v[0].val->obj->ival(v[0].val, &end, 8*sizeof(ival_t), &v[0].epoint);
        break;
    case 3: 
        err = v[2].val->obj->ival(v[2].val, &step, 8*sizeof(ival_t), &v[2].epoint);
        if (err) return err; 
    case 2: 
        err = v[0].val->obj->ival(v[0].val, &start, 8*sizeof(ival_t), &v[0].epoint);
        if (err) return err; 
        err = v[1].val->obj->ival(v[1].val, &end, 8*sizeof(ival_t), &v[1].epoint);
        break;
    }
    if (err) return err;
    if (step == 0) {
        return new_error_obj(ERROR_NO_ZERO_VALUE, &v[2].epoint);
    }
    if (step > 0) {
        if (end < start) end = start;
        len2 = (end - start + step - 1) / step;
    } else {
        if (end > start) end = start;
        len2 = (start - end - step - 1) / -step;
    }
    new_value = val_alloc(LIST_OBJ);
    val = list_create_elements(new_value, len2);
    if (len2) {
        i = 0;
        while ((end > start && step > 0) || (end < start && step < 0)) {
            val[i] = int_from_ival(start);
            i++; start += step;
        }
    }
    new_value->u.list.len = len2;
    new_value->u.list.data = val;
    return new_value;
}

static MUST_CHECK value_t apply_func(value_t v1, enum func_e func, linepos_t epoint) {
    value_t err, v;
    double real;
    switch (func) {
    case F_ANY: return v1->obj->truth(v1, TRUTH_ANY, epoint);
    case F_ALL: return v1->obj->truth(v1, TRUTH_ALL, epoint);
    case F_LEN: return v1->obj->len(v1, epoint);
    default: break;
    }
    if (v1->obj == TUPLE_OBJ || v1->obj == LIST_OBJ) {
        if (v1->u.list.len) {
            int error = 1;
            size_t i;
            value_t *vals;
            v = val_alloc(v1->obj);
            vals = list_create_elements(v, v1->u.list.len);
            for (i = 0; i < v1->u.list.len; i++) {
                value_t val = apply_func(v1->u.list.data[i], func, epoint);
                if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                vals[i] = val;
            }
            v->u.list.len = i;
            v->u.list.data = vals;
            return v;
        }
        return val_reference(v1);
    }
    switch (func) {
    case F_SIZE: return v1->obj->size(v1, epoint);
    case F_SIGN: return v1->obj->sign(v1, epoint);
    case F_ABS: return v1->obj->abs(v1, epoint);
    case F_REPR: return v1->obj->repr(v1, epoint);
    default: break;
    }
    err = FLOAT_OBJ->create(v1, epoint);
    if (err->obj != FLOAT_OBJ) return err;
    real = err->u.real;
    val_destroy(err);
    switch (func) {
    case F_FLOOR: real = floor(real);break;
    case F_CEIL: real = ceil(real);break;
    case F_SQRT: 
        if (real < 0.0) {
            return new_error_obj(ERROR_SQUARE_ROOT_N, epoint);
        }
        real = sqrt(real);
        break;
    case F_LOG10: 
        if (real <= 0.0) {
            return new_error_obj(ERROR_LOG_NON_POSIT, epoint);
        }
        real = log10(real);
        break;
    case F_LOG: 
        if (real <= 0.0) {
            return new_error_obj(ERROR_LOG_NON_POSIT, epoint);
        }
        real = log(real);
        break;
    case F_EXP: real = exp(real);break;
    case F_SIN: real = sin(real);break;
    case F_COS: real = cos(real);break;
    case F_TAN: real = tan(real);break;
    case F_ACOS: 
        if (real < -1.0 || real > 1.0) {
            return new_error_obj(ERROR___MATH_DOMAIN, epoint);
        }
        real = acos(real);
        break;
    case F_ASIN: 
        if (real < -1.0 || real > 1.0) {
            return new_error_obj(ERROR___MATH_DOMAIN, epoint);
        }
        real = asin(real);
        break;
    case F_ATAN: real = atan(real);break;
    case F_CBRT: real = cbrt(real);break;
    case F_ROUND: real = round(real);break;
    case F_TRUNC: real = trunc(real);break;
    case F_FRAC: real -= trunc(real);break;
    case F_RAD: real = real * M_PI / 180.0;break;
    case F_DEG: real = real * 180.0 / M_PI;break;
    case F_COSH: real = cosh(real);break;
    case F_SINH: real = sinh(real);break;
    case F_TANH: real = tanh(real);break;
    default: real = HUGE_VAL; break; /* can't happen */
    }
    return float_from_double2(real, epoint);
}

static MUST_CHECK value_t apply_func2(oper_t op, enum func_e func) {
    value_t err, v, v1 = op->v1, v2 = op->v2, val, *vals;
    double real, real2;

    if (v1->obj == TUPLE_OBJ || v1->obj == LIST_OBJ) {
        size_t i;
        if (v1->obj == v2->obj) {
            if (v1->u.list.len == v2->u.list.len) {
                if (v1->u.list.len) {
                    int error = 1;
                    v = val_alloc(v1->obj);
                    vals = list_create_elements(v, v1->u.list.len);
                    for (i = 0; i < v1->u.list.len; i++) {
                        op->v1 = v1->u.list.data[i];
                        op->v2 = v2->u.list.data[i];
                        val = apply_func2(op, func);
                        if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                        vals[i] = val;
                    }
                    op->v1 = v1;
                    op->v2 = v2;
                    v->u.list.len = i;
                    v->u.list.data = vals;
                    return v;
                } 
                return val_reference(v1);
            } 
            if (v1->u.list.len == 1) {
                op->v1 = v1->u.list.data[0];
                v = apply_func2(op, func);
                op->v1 = v1;
                return v;
            } 
            if (v2->u.list.len == 1) {
                op->v2 = v2->u.list.data[0];
                v = apply_func2(op, func);
                op->v2 = v2;
                return v;
            } 
            v = new_error_obj(ERROR_CANT_BROADCAS, op->epoint3);
            v->u.error.u.broadcast.v1 = v1->u.list.len;
            v->u.error.u.broadcast.v2 = v2->u.list.len;
            return v;
        }
        if (v1->u.list.len) {
            int error = 1;
            v = val_alloc(v1->obj);
            vals = list_create_elements(v, v1->u.list.len);
            for (i = 0; i < v1->u.list.len; i++) {
                op->v1 = v1->u.list.data[i];
                val = apply_func2(op, func);
                if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                vals[i] = val;
            }
            op->v1 = v1;
            v->u.list.len = i;
            v->u.list.data = vals;
            return v;
        }
        return val_reference(v1);
    }
    if (v2->obj == TUPLE_OBJ || v2->obj == LIST_OBJ) {
        if (v2->u.list.len) {
            int  error = 1;
            size_t i;
            v = val_alloc(v2->obj);
            vals = list_create_elements(v, v2->u.list.len);
            for (i = 0; i < v2->u.list.len; i++) {
                op->v2 = v2->u.list.data[i];
                val = apply_func2(op, func);
                if (val->obj == ERROR_OBJ) { if (error) {err_msg_output(val); error = 0;} val_destroy(val); val = val_reference(none_value); }
                vals[i] = val;
            }
            op->v2 = v2;
            v->u.list.len = i;
            v->u.list.data = vals;
            return v;
        }
        return val_reference(v2);
    }
    err = FLOAT_OBJ->create(v1, op->epoint);
    if (err->obj != FLOAT_OBJ) return err;
    real = err->u.real;
    val_destroy(err);
    err = FLOAT_OBJ->create(v2, op->epoint2);
    if (err->obj != FLOAT_OBJ) return err;
    real2 = err->u.real;
    val_destroy(err);
    switch (func) {
    case F_HYPOT: real = hypot(real, real2); break;
    case F_ATAN2: real = atan2(real, real2);break;
    case F_POW:
        if (real2 < 0.0 && !real) {
            return new_error_obj(ERROR_DIVISION_BY_Z, op->epoint3);
        }
        if (real < 0.0 && (double)((int)real2) != real2) {
            return new_error_obj(ERROR_NEGFRAC_POWER, op->epoint3);
        }
        real = pow(real, real2);
        break;
    default: real = HUGE_VAL; break;
    }
    return float_from_double2(real, op->epoint3);
}

static MUST_CHECK value_t calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    enum func_e func;
    struct values_s *v;
    size_t args;
    struct oper_s oper;
    switch (v2->obj->type) {
    case T_FUNCTION:
        switch (op->op->u.oper.op) {
        case O_CMP:
            if (v1->u.function.func < v1->u.function.func) return int_from_int(-1);
            return val_reference(int_value[v1->u.function.func > v2->u.function.func]);
        case O_EQ: return truth_reference(v1->u.function.func == v2->u.function.func);
        case O_NE: return truth_reference(v1->u.function.func != v2->u.function.func);
        case O_LT: return truth_reference(v1->u.function.func < v2->u.function.func);
        case O_LE: return truth_reference(v1->u.function.func <= v2->u.function.func);
        case O_GT: return truth_reference(v1->u.function.func > v2->u.function.func);
        case O_GE: return truth_reference(v1->u.function.func >= v2->u.function.func);
        default: break;
        }
        break;
    case T_FUNCARGS:
        v = v2->u.funcargs.val;
        args = v2->u.funcargs.len;
        switch (op->op->u.oper.op) {
        case O_FUNC:
            func = v1->u.function.func;
            switch (func) {
            case F_HYPOT:
            case F_ATAN2:
            case F_POW: 
                if (args != 2) {
                    err_msg_argnum(args, 2, 2, op->epoint2);
                    return val_reference(none_value);
                }
                oper.v1 = v[0].val;
                oper.v2 = v[1].val;
                oper.epoint = &v[0].epoint;
                oper.epoint2 = &v[1].epoint;
                oper.epoint3 = op->epoint;
                return apply_func2(&oper, func);
            case F_RANGE: return function_range(v2, op->epoint2);
            case F_FORMAT: return isnprintf(v2, op->epoint);
            default:
                if (args != 1) {
                    err_msg_argnum(args, 1, 1, op->epoint2);
                    return val_reference(none_value);
                }
                return apply_func(v[0].val, func, &v[0].epoint);
            }
        default: break;
        }
        break;
    case T_NONE:
    case T_ERROR:
    case T_LIST:
    case T_TUPLE:
    case T_DICT:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

struct builtin_functions_s builtin_functions[] = {
    {"abs", F_ABS}, 
    {"acos", F_ACOS}, 
    {"all", F_ALL},
    {"any", F_ANY},
    {"asin", F_ASIN}, 
    {"atan", F_ATAN}, 
    {"atan2", F_ATAN2}, 
    {"cbrt", F_CBRT}, 
    {"ceil", F_CEIL},
    {"cos", F_COS}, 
    {"cosh", F_COSH}, 
    {"deg", F_DEG}, 
    {"exp", F_EXP}, 
    {"floor", F_FLOOR},
    {"frac", F_FRAC}, 
    {"hypot", F_HYPOT}, 
    {"len", F_LEN},
    {"log", F_LOG},
    {"log10", F_LOG10}, 
    {"pow", F_POW}, 
    {"rad", F_RAD}, 
    {"range", F_RANGE},
    {"repr", F_REPR},
    {"round", F_ROUND},
    {"sign", F_SIGN}, 
    {"sin", F_SIN}, 
    {"sinh", F_SINH}, 
    {"size", F_SIZE},
    {"sqrt", F_SQRT}, 
    {"tan", F_TAN}, 
    {"tanh", F_TANH}, 
    {"trunc", F_TRUNC}, 
    {"format", F_FORMAT}, 
    {NULL, F_NONE}
};

void functionobj_init(void) {
    obj_init(&obj, T_FUNCTION, "function");
    obj.hash = hash;
    obj.same = same;
    obj.repr = repr;
    obj.calc2 = calc2;
}
