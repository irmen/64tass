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

/* return templates only! */
static struct value_s *apply_func(enum func_e func, struct value_s *v1, linepos_t epoint) {
    static struct value_s new_value;
    double real;
    switch (func) {
    case F_ANY:
        v1->obj->truth(v1, &new_value, TRUTH_ANY, epoint);
        return &new_value;
    case F_ALL:
        v1->obj->truth(v1, &new_value, TRUTH_ALL, epoint);
        return &new_value;
    case F_LEN:
        v1->obj->len(v1, &new_value, epoint);
        return &new_value;
    case F_REPR:
        v1->obj->repr(v1, &new_value, epoint);
        return &new_value;
    case F_STR:
        v1->obj->str(v1, &new_value, epoint);
        return &new_value;
    default: break;
    }
    switch (v1->obj->type) {
    case T_LIST:
    case T_TUPLE:
        {
            size_t i = 0;
            struct value_s **vals;
            const struct value_s *val;
            vals = list_create_elements(&new_value, v1->u.list.len);
            if (v1->u.list.len) {
                for (;i < v1->u.list.len; i++) {
                    val = apply_func(func, v1->u.list.data[i], epoint);
                    val_set_template(vals + i, val);
                }
            }
            new_value.obj = v1->obj;
            new_value.u.list.len = i;
            new_value.u.list.data = vals;
            break;
        }
    default:
        switch (func) {
        case F_SIZE:
            v1->obj->size(v1, &new_value, epoint);
            return &new_value;
        case F_SIGN:
            v1->obj->sign(v1, &new_value, epoint);
            return &new_value;
        case F_ABS:
            v1->obj->abs(v1, &new_value, epoint);
            return &new_value;
        case F_INT:
            v1->obj->integer(v1, &new_value, epoint);
            return &new_value;
        case F_BOOL:
            v1->obj->truth(v1, &new_value, TRUTH_BOOL, epoint);
            return &new_value;
        case F_REGISTER:
            function_register(v1, &new_value, epoint);
            return &new_value;
        default: break;
        }
        new_value.obj = FLOAT_OBJ;
        if (!v1->obj->real(v1, &new_value, &real, epoint)) {
            switch (func) {
            case F_FLOOR: real = floor(real);break;
            case F_CEIL: real = ceil(real);break;
            case F_SQRT: if (real < 0.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                         else real = sqrt(real);break;
            case F_LOG10: if (real <= 0.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                          else real = log10(real);break;
            case F_LOG: if (real <= 0.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                        else real = log(real);break;
            case F_EXP: real = exp(real);break;
            case F_SIN: real = sin(real);break;
            case F_COS: real = cos(real);break;
            case F_TAN: real = tan(real);break;
            case F_ACOS: if (real < -1.0 || real > 1.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                         else real = acos(real);break;
            case F_ASIN: if (real < -1.0 || real > 1.0) err_msg2(ERROR_CONSTNT_LARGE, NULL, epoint);
                         else real = asin(real);break;
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
            float_from_double(&new_value, real);
        }
        break;
    case T_NONE: return &none_value;
    }
    return &new_value;
}

static inline void apply_func2(struct values_s *vals, unsigned int args, enum func_e func) {
    struct values_s *v = &vals[2];
    struct value_s *val;
    if (args != 1) err_msg_argnum(args, 1, 1, &vals->epoint);
    else {
        val = apply_func(func, v[0].val, &v[0].epoint);
        if (val->obj == FLOAT_OBJ && val->u.real == HUGE_VAL) {
            val->obj = ERROR_OBJ;
            val->u.error.num = ERROR_CONSTNT_LARGE;
            val->u.error.epoint = vals->epoint;
        }
        val_replace_template(&vals->val, val);
        return;
    }
    val_replace(&vals->val, &none_value);
}

static inline void apply_func3(struct values_s *vals, unsigned int args, enum func_e func) {
    struct value_s new_value;
    double val1, val2;
    struct values_s *v = &vals[2];
    if (args != 2) err_msg_argnum(args, 2, 2, &vals->epoint);
    else {
        if (v[0].val->obj->real(v[0].val, &new_value, &val1, &v[0].epoint)) {
            val_replace_template(&vals->val, &new_value);
            return;
        }
        if (v[1].val->obj->real(v[1].val, &new_value, &val2, &v[1].epoint)) {
            val_replace_template(&vals->val, &new_value);
            return;
        }
        new_value.obj = FLOAT_OBJ;
        switch (func) {
        case F_HYPOT: new_value.u.real = hypot(val1, val2); break;
        case F_ATAN2: new_value.u.real = atan2(val1, val2);break;
        case F_POW:
            if (val2 < 0.0 && !val1) {
                new_value.obj = ERROR_OBJ;
                new_value.u.error.num = ERROR_DIVISION_BY_Z;
                new_value.u.error.epoint = v[1].epoint;
                break;
            }
            else if (val1 < 0.0 && (double)((int)val2) != val2) {
                new_value.obj = ERROR_OBJ;
                new_value.u.error.num = ERROR_NEGFRAC_POWER;
                new_value.u.error.epoint = v[1].epoint;
            } else new_value.u.real = pow(val1, val2);
            break;
        default: new_value.u.real = HUGE_VAL; break;
        }
        if (new_value.obj == FLOAT_OBJ && new_value.u.real == HUGE_VAL) {
            new_value.obj = ERROR_OBJ;
            new_value.u.error.num = ERROR_CONSTNT_LARGE;
            new_value.u.error.epoint = vals->epoint;
        }
        val_replace_template(&vals->val, &new_value);
        return;
    }
    val_replace(&vals->val, &none_value);
}

extern void builtin_function(struct values_s *vals, unsigned int args, enum func_e func) {
    switch (func) {
    case F_HYPOT:
    case F_ATAN2:
    case F_POW: return apply_func3(vals, args, func);
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
    case F_SIZE: return apply_func2(vals, args, func);
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
