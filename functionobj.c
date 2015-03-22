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
#include "isnprintf.h"
#include "functionobj.h"
#include "eval.h"
#include "misc.h"
#include "variables.h"
#include "floatobj.h"
#include "strobj.h"
#include "error.h"
#include "listobj.h"
#include "floatobj.h"
#include "intobj.h"
#include "boolobj.h"
#include "operobj.h"

static struct obj_s obj;

obj_t FUNCTION_OBJ = &obj;

static int same(const Obj *o1, const Obj *o2) {
    const Function *v1 = (const Function *)o1, *v2 = (const Function *)o2;
    return o2->obj == FUNCTION_OBJ && v1->func == v2->func;
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Function *v1 = (Function *)o1;
    *hs = v1->name_hash;
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint) {
    const Function *v1 = (const Function *)o1;
    uint8_t *s;
    const char *prefix;
    size_t len;
    Str *v;
    if (!epoint) return NULL;
    prefix = "<native_function '";
    len = strlen(prefix);
    v = new_str();
    v->len = v1->name.len + 2 + len;
    if (v->len < (2 + len)) err_msg_out_of_memory(); /* overflow */
    v->chars = v->len;
    s = str_create_elements(v, v->len);
    memcpy(s, prefix, len);
    memcpy(s + len, v1->name.data, v1->name.len);
    s[v->len - 2] = '\'';
    s[v->len - 1] = '>';
    v->data = s;
    return &v->v;
}

/* range([start],end,[step]) */
static inline MUST_CHECK Obj *function_range(Funcargs *vals, linepos_t epoint) {
    struct values_s *v = vals->val;
    size_t args = vals->len;
    List *new_value;
    Error *err;
    ival_t start = 0, end, step = 1;
    size_t i = 0, len2;
    Obj **val;

    if (args < 1 || args > 3) {
        err_msg_argnum(args, 1, 3, epoint);
        return (Obj *)ref_none();
    }
    switch (args) {
    case 1: 
        err = v[0].val->obj->ival(v[0].val, &end, 8*sizeof(ival_t), &v[0].epoint);
        break;
    case 3: 
        err = v[2].val->obj->ival(v[2].val, &step, 8*sizeof(ival_t), &v[2].epoint);
        if (err) return &err->v; 
    case 2: 
        err = v[0].val->obj->ival(v[0].val, &start, 8*sizeof(ival_t), &v[0].epoint);
        if (err) return &err->v; 
        err = v[1].val->obj->ival(v[1].val, &end, 8*sizeof(ival_t), &v[1].epoint);
        break;
    }
    if (err) return &err->v;
    if (step == 0) {
        return (Obj *)new_error(ERROR_NO_ZERO_VALUE, &v[2].epoint);
    }
    if (step > 0) {
        if (end < start) end = start;
        len2 = (end - start + step - 1) / step;
    } else {
        if (end > start) end = start;
        len2 = (start - end - step - 1) / -step;
    }
    new_value = new_list();
    val = list_create_elements(new_value, len2);
    if (len2) {
        i = 0;
        while ((end > start && step > 0) || (end < start && step < 0)) {
            val[i] = (Obj *)int_from_ival(start);
            i++; start += step;
        }
    }
    new_value->len = len2;
    new_value->data = val;
    return &new_value->v;
}

static MUST_CHECK Obj *apply_func(Obj *o1, enum func_e func, linepos_t epoint) {
    Obj *err;
    double real;
    switch (func) {
    case F_ANY: return o1->obj->truth(o1, TRUTH_ANY, epoint);
    case F_ALL: return o1->obj->truth(o1, TRUTH_ALL, epoint);
    case F_LEN: return o1->obj->len(o1, epoint);
    default: break;
    }
    if (o1->obj == TUPLE_OBJ || o1->obj == LIST_OBJ) {
        List *v1 = (List *)o1, *v;
        if (v1->len) {
            int error = 1;
            size_t i;
            Obj **vals;
            v = (List *)val_alloc(o1->obj);
            vals = list_create_elements(v, v1->len);
            for (i = 0; i < v1->len; i++) {
                Obj *val = apply_func(v1->data[i], func, epoint);
                if (val->obj == ERROR_OBJ) { if (error) {err_msg_output((Error *)val); error = 0;} val_destroy(val); val = (Obj *)ref_none(); }
                vals[i] = val;
            }
            v->len = i;
            v->data = vals;
            return &v->v;
        }
        return val_reference(&v1->v);
    }
    switch (func) {
    case F_SIZE: return o1->obj->size(o1, epoint);
    case F_SIGN: return o1->obj->sign(o1, epoint);
    case F_ABS: return o1->obj->abs(o1, epoint);
    case F_REPR: return o1->obj->repr(o1, epoint);
    default: break;
    }
    err = FLOAT_OBJ->create(o1, epoint);
    if (err->obj != FLOAT_OBJ) return err;
    real = ((Float *)err)->real;
    val_destroy(err);
    switch (func) {
    case F_FLOOR: real = floor(real);break;
    case F_CEIL: real = ceil(real);break;
    case F_SQRT: 
        if (real < 0.0) {
            return (Obj *)new_error(ERROR_SQUARE_ROOT_N, epoint);
        }
        real = sqrt(real);
        break;
    case F_LOG10: 
        if (real <= 0.0) {
            return (Obj *)new_error(ERROR_LOG_NON_POSIT, epoint);
        }
        real = log10(real);
        break;
    case F_LOG: 
        if (real <= 0.0) {
            return (Obj *)new_error(ERROR_LOG_NON_POSIT, epoint);
        }
        real = log(real);
        break;
    case F_EXP: real = exp(real);break;
    case F_SIN: real = sin(real);break;
    case F_COS: real = cos(real);break;
    case F_TAN: real = tan(real);break;
    case F_ACOS: 
        if (real < -1.0 || real > 1.0) {
            return (Obj *)new_error(ERROR___MATH_DOMAIN, epoint);
        }
        real = acos(real);
        break;
    case F_ASIN: 
        if (real < -1.0 || real > 1.0) {
            return (Obj *)new_error(ERROR___MATH_DOMAIN, epoint);
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
    return float_from_double(real, epoint);
}

static MUST_CHECK Obj *apply_func2(oper_t op, enum func_e func) {
    Obj *o1 = op->v1, *o2 = op->v2, *val, **vals;
    Error *err;
    double real, real2;

    if (o1->obj == TUPLE_OBJ || o1->obj == LIST_OBJ) {
        List *v1 = (List *)o1;
        size_t i;
        if (o1->obj == o2->obj) {
            List *v2 = (List *)o2;
            if (v1->len == v2->len) {
                if (v1->len) {
                    int error = 1;
                    List *v = (List *)val_alloc(o1->obj);
                    vals = list_create_elements(v, v1->len);
                    for (i = 0; i < v1->len; i++) {
                        op->v1 = v1->data[i];
                        op->v2 = v2->data[i];
                        val = apply_func2(op, func);
                        if (val->obj == ERROR_OBJ) { if (error) {err_msg_output((Error *)val); error = 0;} val_destroy(val); val = (Obj *)ref_none(); }
                        vals[i] = val;
                    }
                    op->v1 = (Obj *)v1;
                    op->v2 = (Obj *)v2;
                    v->len = i;
                    v->data = vals;
                    return (Obj *)v;
                } 
                return val_reference(&v1->v);
            } 
            if (v1->len == 1) {
                Obj *v;
                op->v1 = v1->data[0];
                v = apply_func2(op, func);
                op->v1 = (Obj *)v1;
                return v;
            } 
            if (v2->len == 1) {
                Obj *v;
                op->v2 = v2->data[0];
                v = apply_func2(op, func);
                op->v2 = (Obj *)v2;
                return v;
            } 
            err = new_error(ERROR_CANT_BROADCAS, op->epoint3);
            err->u.broadcast.v1 = v1->len;
            err->u.broadcast.v2 = v2->len;
            return &err->v;
        }
        if (v1->len) {
            int error = 1;
            List *v = (List *)val_alloc(o1->obj);
            vals = list_create_elements(v, v1->len);
            for (i = 0; i < v1->len; i++) {
                op->v1 = v1->data[i];
                val = apply_func2(op, func);
                if (val->obj == ERROR_OBJ) { if (error) {err_msg_output((Error *)val); error = 0;} val_destroy(val); val = (Obj *)ref_none(); }
                vals[i] = val;
            }
            op->v1 = (Obj *)v1;
            v->len = i;
            v->data = vals;
            return &v->v;
        }
        return val_reference(&v1->v);
    }
    if (o2->obj == TUPLE_OBJ || o2->obj == LIST_OBJ) {
        List *v2 = (List *)o2;
        if (v2->len) {
            int  error = 1;
            size_t i;
            List *v = (List *)val_alloc(o2->obj);
            vals = list_create_elements(v, v2->len);
            for (i = 0; i < v2->len; i++) {
                op->v2 = v2->data[i];
                val = apply_func2(op, func);
                if (val->obj == ERROR_OBJ) { if (error) {err_msg_output((Error *)val); error = 0;} val_destroy(val); val = (Obj *)ref_none(); }
                vals[i] = val;
            }
            op->v2 = (Obj *)v2;
            v->len = i;
            v->data = vals;
            return &v->v;
        }
        return val_reference(&v2->v);
    }
    val = FLOAT_OBJ->create(o1, op->epoint);
    if (val->obj != FLOAT_OBJ) return val;
    real = ((Float *)val)->real;
    val_destroy(val);
    val = FLOAT_OBJ->create(o2, op->epoint2);
    if (val->obj != FLOAT_OBJ) return val;
    real2 = ((Float *)val)->real;
    val_destroy(val);
    switch (func) {
    case F_HYPOT: real = hypot(real, real2); break;
    case F_ATAN2: real = atan2(real, real2);break;
    case F_POW:
        if (real2 < 0.0 && !real) {
            return (Obj *)new_error(ERROR_DIVISION_BY_Z, op->epoint3);
        }
        if (real < 0.0 && (double)((int)real2) != real2) {
            return (Obj *)new_error(ERROR_NEGFRAC_POWER, op->epoint3);
        }
        real = pow(real, real2);
        break;
    default: real = HUGE_VAL; break;
    }
    return float_from_double(real, op->epoint3);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Function *v1 = (Function *)op->v1;
    Obj *o2 = op->v2;
    enum func_e func;
    struct values_s *v;
    size_t args;
    struct oper_s oper;
    switch (o2->obj->type) {
    case T_FUNCTION:
        {
            Function *v2 = (Function *)o2;
            switch (op->op->op) {
            case O_CMP:
                if (v1->func < v1->func) return (Obj *)ref_int(minus1_value);
                return (Obj *)ref_int(int_value[v1->func > v2->func]);
            case O_EQ: return truth_reference(v1->func == v2->func);
            case O_NE: return truth_reference(v1->func != v2->func);
            case O_LT: return truth_reference(v1->func < v2->func);
            case O_LE: return truth_reference(v1->func <= v2->func);
            case O_GT: return truth_reference(v1->func > v2->func);
            case O_GE: return truth_reference(v1->func >= v2->func);
            default: break;
            }
            break;
        }
    case T_FUNCARGS:
        {
            Funcargs *v2 = (Funcargs *)o2;
            v = v2->val;
            args = v2->len;
            switch (op->op->op) {
            case O_FUNC:
                func = v1->func;
                switch (func) {
                case F_HYPOT:
                case F_ATAN2:
                case F_POW: 
                    if (args != 2) {
                        err_msg_argnum(args, 2, 2, op->epoint2);
                        return (Obj *)ref_none();
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
                                   return (Obj *)ref_none();
                               }
                               return apply_func(v[0].val, func, &v[0].epoint);
                }
            default: break;
            }
            break;
        }
    case T_NONE:
    case T_ERROR:
    case T_LIST:
    case T_TUPLE:
    case T_DICT:
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return o2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

void functionobj_init(void) {
    obj_init(&obj, T_FUNCTION, "function", sizeof(Function));
    obj.hash = hash;
    obj.same = same;
    obj.repr = repr;
    obj.calc2 = calc2;
}

struct builtin_functions_s {
    const char *name;
    enum func_e func;
};

static struct builtin_functions_s builtin_functions[] = {
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

void functionobj_names(void) {
    int i;

    for (i = 0; builtin_functions[i].name; i++) {
        Function *func = new_function();
        func->name.data = (const uint8_t *)builtin_functions[i].name;
        func->name.len = strlen(builtin_functions[i].name);
        func->func = builtin_functions[i].func;
        func->name_hash = str_hash(&func->name);
        new_builtin(builtin_functions[i].name, &func->v);
    }
}
