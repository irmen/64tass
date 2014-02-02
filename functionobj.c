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
#include "functionobj.h"
#include "eval.h"
#include "variables.h"
#include "floatobj.h"
#include "boolobj.h"
#include "listobj.h"

static struct obj_s obj;

obj_t FUNCTION_OBJ = &obj;

static int same(const struct value_s *v1, const struct value_s *v2) {
    return v2->obj == FUNCTION_OBJ && v1->u.function.func == v2->u.function.func;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    return v1->u.function.name_hash;
}

static void repr(const struct value_s *v1, struct value_s *v, linepos_t UNUSED(epoint)) {
    uint8_t *s;
    const char *prefix = "<native_function '";
    size_t len = strlen(prefix);
    v->obj = STR_OBJ;
    v->u.str.len = v1->u.function.name.len + 2 + len;
    if (v->u.str.len < (2 + len)) err_msg_out_of_memory(); /* overflow */
    v->u.str.chars = v->u.str.len;
    s = str_create_elements(v, v->u.str.len);
    memcpy(s, prefix, len);
    memcpy(s + len, v1->u.function.name.data, v1->u.function.name.len);
    s[v->u.str.len - 2] = '\'';
    s[v->u.str.len - 1] = '>';
    v->u.str.data = s;
    return;
}

/* range([start],end,[step]) */
static inline void function_range(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    struct value_s new_value;
    ival_t start = 0, end, step = 1;
    size_t i = 0, len2;
    struct value_s **val;
    if (args < 1 || args > 3) err_msg_argnum(args, 1, 3, &vals->epoint);
    else {
        switch (args) {
        case 1: if (v[0].val->obj->ival(v[0].val, &new_value, &end, 8*sizeof(ival_t), &v[0].epoint)) { val_replace_template(&vals->val, &new_value);return;} break;
        case 3: if (v[2].val->obj->ival(v[2].val, &new_value, &step, 8*sizeof(ival_t), &v[2].epoint)) { val_replace_template(&vals->val, &new_value);return;}
        case 2: if (v[0].val->obj->ival(v[0].val, &new_value, &start, 8*sizeof(ival_t), &v[0].epoint)) { val_replace_template(&vals->val, &new_value);return;}
                    if (v[1].val->obj->ival(v[1].val, &new_value, &end, 8*sizeof(ival_t), &v[1].epoint)) { val_replace_template(&vals->val, &new_value);return;} break;
        }
        if (step == 0) {
            new_value.obj = ERROR_OBJ;
            new_value.u.error.num = ERROR_DIVISION_BY_Z;
            new_value.u.error.epoint = v[2].epoint;
            val_replace_template(&vals->val, &new_value); return;
        }
        if (step > 0) {
            if (end < start) end = start;
            len2 = (end - start + step - 1) / step;
        } else {
            if (end > start) end = start;
            len2 = (start - end - step - 1) / -step;
        }
        val = list_create_elements(&new_value, len2);
        if (len2) {
            i = 0;
            while ((end > start && step > 0) || (end < start && step < 0)) {
                val[i] = val_alloc();
                int_from_ival(val[i], start);
                i++; start += step;
            }
        }
        new_value.obj = LIST_OBJ;
        new_value.u.list.len = len2;
        new_value.u.list.data = val;
        val_replace_template(&vals->val, &new_value);
        return;
    }
    val_replace(&vals->val, &none_value);
}

/* register(a) - create register object */
static inline void function_register(struct value_s *v1, struct value_s *v, linepos_t epoint) {
    if (v1->obj != NONE_OBJ) {
        if (v1->obj != STR_OBJ) err_msg_wrong_type(v1, epoint);
        else {
            if (v1 != v) STR_OBJ->copy(v1, v);
            v->obj = REGISTER_OBJ;
            return;
        }
        if (v1 == v) v->obj->destroy(v);
    }
    v->obj = NONE_OBJ;
} 

static void apply_func(struct value_s *v1, struct value_s *v, enum func_e func, linepos_t epoint) {
    double real;
    switch (func) {
    case F_ANY: v1->obj->truth(v1, v, TRUTH_ANY, epoint); return;
    case F_ALL: v1->obj->truth(v1, v, TRUTH_ALL, epoint); return;
    case F_LEN: return v1->obj->len(v1, v, epoint);
    default: break;
    }
    switch (v1->obj->type) {
    case T_LIST:
    case T_TUPLE:
        {
            size_t i = 0;
            struct value_s **vals;
            if (v == v1) {
                for (;i < v1->u.list.len; i++) {
                    struct value_s *val = v1->u.list.data[i];
                    if (val->refcount != 1) {
                        struct value_s *tmp = val_alloc();
                        apply_func(val, tmp, func, epoint);
                        val_destroy(val); v1->u.list.data[i] = tmp;
                    } else apply_func(val, val, func, epoint);
                }
                return;
            }
            vals = list_create_elements(v, v1->u.list.len);
            for (;i < v1->u.list.len; i++) {
                vals[i] = val_alloc();
                apply_func(v1->u.list.data[i], vals[i], func, epoint);
            }
            v->obj = v1->obj;
            v->u.list.len = i;
            v->u.list.data = vals;
            return;
        }
    default:
        switch (func) {
        case F_BOOL: v1->obj->truth(v1, v, TRUTH_BOOL, epoint);return;
        case F_SIZE: return v1->obj->size(v1, v, epoint);
        case F_SIGN: return v1->obj->sign(v1, v, epoint);
        case F_ABS: return v1->obj->abs(v1, v, epoint);
        case F_INT: return v1->obj->integer(v1, v, epoint);
        case F_REGISTER: return function_register(v1, v, epoint);
        case F_REPR: return v1->obj->repr(v1, v, epoint);
        case F_STR: return v1->obj->str(v1, v, epoint);
        default: break;
        }
        if (!v1->obj->real(v1, v, &real, epoint)) {
            switch (func) {
            case F_FLOOR: real = floor(real);break;
            case F_CEIL: real = ceil(real);break;
            case F_SQRT: real = (real < 0.0) ? HUGE_VAL : sqrt(real);break;
            case F_LOG10: real = (real <= 0.0) ? HUGE_VAL : log10(real);break;
            case F_LOG: real = (real <= 0.0) ? HUGE_VAL : log(real);break;
            case F_EXP: real = exp(real);break;
            case F_SIN: real = sin(real);break;
            case F_COS: real = cos(real);break;
            case F_TAN: real = tan(real);break;
            case F_ACOS: real = (real < -1.0 || real > 1.0) ? HUGE_VAL : acos(real);break;
            case F_ASIN: real = (real < -1.0 || real > 1.0) ? HUGE_VAL : asin(real);break;
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
            case F_FLOAT: break; /* nothing to do */
            default: real = HUGE_VAL; break;
            }
            if (v1 == v) v->obj->destroy(v);
            if (real == HUGE_VAL) {
                v->obj = ERROR_OBJ;
                v->u.error.num = ERROR_CONSTNT_LARGE;
                v->u.error.epoint = *epoint;
            } else float_from_double(v, real);
        }
    }
}

static inline void apply_func_single(struct values_s *vals, unsigned int args, enum func_e func) {
    struct values_s *v = &vals[2];
    struct value_s *val;

    if (args != 1) {
        err_msg_argnum(args, 1, 1, &vals->epoint);
        val_destroy(vals->val);
        vals->val = &none_value;
        return;
    }
    if (vals->val->refcount != 1) {
        if (v[0].val->refcount == 1) {
            val = v[0].val;
            v[0].val = vals->val;
            vals->val = val;
            return apply_func(val, val, func, &v[0].epoint);
        }
        val = val_alloc();
        apply_func(v[0].val, val, func, &v[0].epoint);
        val_destroy(vals->val); vals->val = val;
        return;
    } 
    vals->val->obj->destroy(vals->val);
    apply_func(v[0].val, vals->val, func, &v[0].epoint);
}

static void apply_func2(struct value_s *v1, struct value_s *v2, struct value_s *v, enum func_e func, linepos_t epoint, linepos_t epoint2) {
    double real, real2;
    switch (v1->obj->type) {
    case T_LIST:
    case T_TUPLE:
        {
            size_t i = 0;
            struct value_s **vals;
            if (v == v1) {
                for (;i < v1->u.list.len; i++) {
                    struct value_s *val = v1->u.list.data[i];
                    if (val->refcount != 1) {
                        struct value_s *tmp = val_alloc();
                        apply_func2(val, v2, tmp, func, epoint, epoint2);
                        val_destroy(val); v1->u.list.data[i] = tmp;
                    } else apply_func2(val, v2, val, func, epoint, epoint2);
                }
                return;
            }
            vals = list_create_elements(v, v1->u.list.len);
            for (;i < v1->u.list.len; i++) {
                vals[i] = val_alloc();
                apply_func2(v1->u.list.data[i], v2, vals[i], func, epoint, epoint2);
            }
            if (v2 == v) v->obj->destroy(v);
            v->obj = v1->obj;
            v->u.list.len = i;
            v->u.list.data = vals;
            return;
        }
    default: break;
    }
    switch (v2->obj->type) {
    case T_LIST:
    case T_TUPLE:
        {
            size_t i = 0;
            struct value_s **vals;
            if (v == v2) {
                for (;i < v2->u.list.len; i++) {
                    struct value_s *val = v2->u.list.data[i];
                    if (val->refcount != 1) {
                        struct value_s *tmp = val_alloc();
                        apply_func2(v1, val, tmp, func, epoint, epoint2);
                        val_destroy(val); v2->u.list.data[i] = tmp;
                    } else apply_func2(v1, val, val, func, epoint, epoint2);
                }
                return;
            }
            vals = list_create_elements(v, v2->u.list.len);
            for (;i < v2->u.list.len; i++) {
                vals[i] = val_alloc();
                apply_func2(v1, v2->u.list.data[i], vals[i], func, epoint, epoint2);
            }
            if (v1 == v) v->obj->destroy(v);
            v->obj = v2->obj;
            v->u.list.len = i;
            v->u.list.data = vals;
            return;
        }
    default: break;
    }
    if (!v1->obj->real(v1, v, &real, epoint) && !v2->obj->real(v2, v, &real2, epoint2)) {
        switch (func) {
        case F_HYPOT: real = hypot(real, real2); break;
        case F_ATAN2: real = atan2(real, real2);break;
        case F_POW:
                      if (real2 < 0.0 && !real) {
                          if (v1 == v || v2 == v) v->obj->destroy(v);
                          v->obj = ERROR_OBJ;
                          v->u.error.num = ERROR_DIVISION_BY_Z;
                          v->u.error.epoint = *epoint2;
                          return;
                      }
                      if (real < 0.0 && (double)((int)real2) != real2) {
                          if (v1 == v || v2 == v) v->obj->destroy(v);
                          v->obj = ERROR_OBJ;
                          v->u.error.num = ERROR_NEGFRAC_POWER;
                          v->u.error.epoint = *epoint2;
                          return;
                      }
                      real = pow(real, real2);
                      break;
        default: real = HUGE_VAL; break;
        }
        if (v1 == v || v2 == v) v->obj->destroy(v);
        if (real == HUGE_VAL) {
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_CONSTNT_LARGE;
            v->u.error.epoint = *epoint;
        } else float_from_double(v, real);
    }
}

static inline void apply_func_double(struct values_s *vals, unsigned int args, enum func_e func) {
    struct values_s *v = &vals[2];
    struct value_s *val;

    if (args != 2) {
        err_msg_argnum(args, 2, 2, &vals->epoint);
        val_destroy(vals->val);
        vals->val = &none_value;
        return;
    }
    if ((v[0].val->obj == TUPLE_OBJ || v[0].val->obj == LIST_OBJ) && (v[1].val->obj == TUPLE_OBJ || v[1].val->obj == LIST_OBJ)) {
        err_msg_wrong_type(v[0].val, &v[0].epoint);
        val_destroy(vals->val);
        vals->val = &none_value;
        return;
    }
    if (vals->val->refcount != 1) {
        if (v[0].val->refcount == 1 && (v[1].val->refcount != 1 || (v[1].val->obj != LIST_OBJ && v[1].val->obj != TUPLE_OBJ))) {
            val = v[0].val;
            v[0].val = vals->val;
            vals->val = val;
            return apply_func2(val, v[1].val, val, func, &v[0].epoint, &v[1].epoint);
        }
        if (v[1].val->refcount == 1) {
            val = v[1].val;
            v[1].val = vals->val;
            vals->val = val;
            return apply_func2(v[0].val, val, val, func, &v[0].epoint, &v[1].epoint);
        }
        val = val_alloc();
        apply_func2(v[0].val, v[1].val, val, func, &v[0].epoint, &v[1].epoint);
        val_destroy(vals->val); vals->val = val;
    } else {
        vals->val->obj->destroy(vals->val);
        apply_func2(v[0].val, v[1].val, vals->val, func, &v[0].epoint, &v[1].epoint);
    }
}

extern void builtin_function(struct values_s *vals, unsigned int args, enum func_e func) {
    switch (func) {
    case F_HYPOT:
    case F_ATAN2:
    case F_POW: return apply_func_double(vals, args, func);
    case F_LOG:
    case F_EXP:
    case F_SIN:
    case F_COS:
    case F_TAN:
    case F_RAD:
    case F_DEG:
    case F_ABS:
    case F_INT:
    case F_ALL:
    case F_ANY:
    case F_CEIL:
    case F_FRAC:
    case F_SQRT:
    case F_ACOS:
    case F_ASIN:
    case F_ATAN:
    case F_CBRT:
    case F_COSH:
    case F_SINH:
    case F_TANH:
    case F_SIGN:
    case F_BOOL:
    case F_FLOOR:
    case F_ROUND:
    case F_TRUNC:
    case F_LOG10:
    case F_FLOAT:
    case F_STR:
    case F_REPR:
    case F_LEN:
    case F_REGISTER:
    case F_SIZE: return apply_func_single(vals, args, func);
    case F_RANGE: return function_range(vals, args);
    case F_NONE: return;
    }
}

struct builtin_functions_s builtin_functions[] = {
    {"abs", F_ABS}, 
    {"acos", F_ACOS}, 
    {"all", F_ALL},
    {"any", F_ANY},
    {"asin", F_ASIN}, 
    {"atan", F_ATAN}, 
    {"atan2", F_ATAN2}, 
    {"bool", F_BOOL}, 
    {"cbrt", F_CBRT}, 
    {"ceil", F_CEIL},
    {"cos", F_COS}, 
    {"cosh", F_COSH}, 
    {"deg", F_DEG}, 
    {"exp", F_EXP}, 
    {"float", F_FLOAT}, 
    {"floor", F_FLOOR},
    {"frac", F_FRAC}, 
    {"hypot", F_HYPOT}, 
    {"int", F_INT},
    {"len", F_LEN},
    {"log", F_LOG},
    {"log10", F_LOG10}, 
    {"pow", F_POW}, 
    {"rad", F_RAD}, 
    {"range", F_RANGE},
    {"register", F_REGISTER},
    {"repr", F_REPR},
    {"round", F_ROUND},
    {"sign", F_SIGN}, 
    {"sin", F_SIN}, 
    {"sinh", F_SINH}, 
    {"size", F_SIZE},
    {"sqrt", F_SQRT}, 
    {"str", F_STR},
    {"tan", F_TAN}, 
    {"tanh", F_TANH}, 
    {"trunc", F_TRUNC}, 
    {NULL, F_NONE}
};

void functionobj_init(void) {
    obj_init(&obj, T_FUNCTION, "<function>");
    obj.hash = hash;
    obj.same = same;
    obj.repr = repr;
}
