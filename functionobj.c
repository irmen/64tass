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
    return v2->obj == FUNCTION_OBJ && v1->u.function.call == v2->u.function.call;
}

static int hash(const struct value_s *v1, struct value_s *UNUSED(v), linepos_t UNUSED(epoint)) {
    return (int)v1->u.function.call & (((unsigned int)~0) >> 1);
}

static void repr(const struct value_s *v1, struct value_s *v) {
    uint8_t *s;
    const char *prefix = "<native_function '";
    size_t len = strlen(prefix);
    v->obj = STR_OBJ;
    v->u.str.len = v1->u.function.name.len + 2 + len;
    v->u.str.chars = v->u.str.len;
    s = (uint8_t *)malloc(v->u.str.len);
    if (!s || v->u.str.len < (2 + len)) err_msg_out_of_memory(); /* overflow */
    memcpy(s, prefix, len);
    memcpy(s + len, v1->u.function.name.data, v1->u.function.name.len);
    s[v->u.str.len - 2] = '\'';
    s[v->u.str.len - 1] = '>';
    v->u.str.data = s;
    return;
}

/* len(a) - length of string in characters */
static void function_len(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    struct value_s new_value;

    if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, &vals->epoint); else {
        uval_t uv;
        if (!v[0].val->obj->len(v[0].val, &new_value, &uv, &vals->epoint)) int_from_uval(&new_value, uv);
        val_replace_template(&vals->val, &new_value);
        return;
    }
    val_replace(&vals->val, &none_value);
}

/* range([start],end,[step]) */
static void function_range(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    struct value_s new_value;
    ival_t start = 0, end, step = 1;
    size_t i = 0, len2;
    struct value_s **val;
    if (args < 1 || args > 3) err_msg2(ERROR_ILLEGAL_OPERA,NULL, &vals->epoint); else {
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
        if (len2) {
            val = (struct value_s **)malloc(len2 * sizeof(new_value.u.list.data[0]));
            if (!val || len2 > ((size_t)~0) / sizeof(new_value.u.list.data[0])) err_msg_out_of_memory(); /* overflow */
            i = 0;
            while ((end > start && step > 0) || (end < start && step < 0)) {
                val[i] = val_alloc();
                int_from_ival(val[i], start);
                i++; start += step;
            }
        } else val = NULL;
        new_value.obj = LIST_OBJ;
        new_value.u.list.len = len2;
        new_value.u.list.data = val;
        val_replace_template(&vals->val, &new_value);
        return;
    }
    val_replace(&vals->val, &none_value);
}

/* repr(a) - representation */
static void function_repr(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    struct value_s new_value;
    if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, &vals->epoint);
    else if (v[0].val->obj != NONE_OBJ) {
        v[0].val->obj->repr(v[0].val, &new_value);
        val_replace_template(&vals->val, &new_value);
        return;
    }
    val_replace(&vals->val, &none_value);
}

/* str(a) - string */
static void function_str(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    struct value_s new_value;
    if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, &vals->epoint);
    else  if (v[0].val->obj != NONE_OBJ) {
        v[0].val->obj->str(v[0].val, &new_value);
        val_replace_template(&vals->val, &new_value);
        return;
    }
    val_replace(&vals->val, &none_value);
} 

/* register(a) - create register object */
static void function_register(struct values_s *vals, unsigned int args) {
    struct values_s *v = &vals[2];
    struct value_s new_value;

    if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, &vals->epoint);
    else if (v[0].val->obj != NONE_OBJ) {
        if (v[0].val->obj != STR_OBJ) err_msg_wrong_type(v[0].val, &v[0].epoint);
        else {
            uint8_t *s;
            new_value.obj = REGISTER_OBJ;
            new_value.u.reg.len = v[0].val->u.str.len;
            new_value.u.reg.chars = v[0].val->u.str.chars;
            s = (uint8_t *)malloc(new_value.u.reg.len);
            if (!s) err_msg_out_of_memory();
            memcpy(s, v[0].val->u.str.data, new_value.u.reg.len);
            new_value.u.reg.data = s;
            val_replace_template(&vals->val, &new_value);
            return;
        }
    }
    val_replace(&vals->val, &none_value);
} 

enum func_e {
    F_NONE, F_FLOOR, F_CEIL, F_ROUND, F_TRUNC, F_FRAC, F_SQRT, F_CBRT, F_LOG,
    F_LOG10, F_EXP, F_SIN, F_COS, F_TAN, F_ACOS, F_ASIN, F_ATAN, F_RAD, F_DEG,
    F_COSH, F_SINH, F_TANH, F_HYPOT, F_ATAN2, F_POW, F_SIGN, F_ABS, F_FLOAT,
    F_INT, F_ALL, F_ANY, F_BOOL, F_SIZE
};

/* return templates only! */
static struct value_s *apply_func(enum func_e func, struct value_s *v1, linepos_t epoint) {
    static struct value_s new_value;
    double real;
    int truth;
    switch (func) {
    case F_SIZE:
        switch (v1->obj->type) {
        case T_LIST:
        case T_TUPLE: break;
        default:
            {
                uval_t uv;
                if (!v1->obj->size(v1, &new_value, &uv, epoint)) int_from_uval(&new_value, uv);
                return &new_value;
            }
        case T_NONE: return &none_value;
        }
        break;
    case F_SIGN:
        switch (v1->obj->type) {
        case T_LIST:
        case T_TUPLE: break;
        default:
            {
                int s;
                if (!v1->obj->sign(v1, &new_value, &s, epoint)) int_from_int(&new_value, s);
                return &new_value;
            }
        case T_NONE: return &none_value;
        }
        break;
    case F_ABS:
        switch (v1->obj->type) {
        case T_LIST:
        case T_TUPLE: break;
        default: 
            v1->obj->abs(v1, &new_value, epoint);
            return &new_value;
        case T_NONE: return &none_value;
        }
        break;
    case F_INT:
        switch (v1->obj->type) { 
        case T_LIST:
        case T_TUPLE: break;
        default:
            v1->obj->integer(v1, &new_value, epoint);
            return &new_value;
        case T_NONE: return &none_value;
        }
        break;
    case F_BOOL:
        switch (v1->obj->type) { 
        case T_LIST:
        case T_TUPLE: break;
        default:
            if (!v1->obj->truth(v1, &new_value, &truth, TRUTH_BOOL, epoint)) bool_from_int(&new_value, truth);
            return &new_value;
        case T_NONE: return &none_value;
        }
        break;
    case F_ANY:
        if (!v1->obj->truth(v1, &new_value, &truth, TRUTH_ANY, epoint)) bool_from_int(&new_value, truth);
        return &new_value;
    case F_ALL:
        if (!v1->obj->truth(v1, &new_value, &truth, TRUTH_ALL, epoint)) bool_from_int(&new_value, truth);
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
                if (v1->u.list.len) {
                    vals = (struct value_s **)malloc(v1->u.list.len * sizeof(new_value.u.list.data[0]));
                    if (!vals) err_msg_out_of_memory();
                    for (;i < v1->u.list.len; i++) {
                        val = apply_func(func, v1->u.list.data[i], epoint);
                        val_set_template(vals + i, val);
                    }
                } else vals = NULL;
                new_value.obj = v1->obj;
                new_value.u.list.len = i;
                new_value.u.list.data = vals;
                break;
            }
    default:
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
            case F_FLOAT: real = HUGE_VAL; break; /* nothing to do */
            default:break;
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
    if (args != 1) err_msg2(ERROR_ILLEGAL_OPERA,NULL, &vals->epoint);
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

/* log(a) */
static void function_log(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_LOG);
} 

static void function_exp(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_EXP);
};

static void function_sin(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_SIN);
};

static void function_cos(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_COS);
};

static void function_tan(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_TAN);
};

static void function_rad(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_RAD);
};

static void function_deg(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_DEG);
};

static void function_abs(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_ABS);
};

static void function_int(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_INT);
};

static void function_all(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_ALL);
};

static void function_any(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_ANY);
};

static void function_ceil(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_CEIL);
}

static void function_frac(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_FRAC);
}

static void function_sqrt(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_SQRT);
}

static void function_acos(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_ACOS);
}

static void function_asin(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_ASIN);
}

static void function_atan(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_ATAN);
}

static void function_cbrt(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_CBRT);
}

static void function_cosh(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_COSH);
}

static void function_sinh(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_SINH);
}

static void function_tanh(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_TANH);
}

static void function_sign(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_SIGN);
}

static void function_bool(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_BOOL);
}

static void function_floor(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_FLOOR);
}

static void function_round(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_ROUND);
}

static void function_trunc(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_TRUNC);
}

static void function_log10(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_LOG10);
}

static void function_float(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_FLOAT);
}

/* size(a) - size of data structure at location */
static void function_size(struct values_s *vals, unsigned int args) {
    return apply_func2(vals, args, F_SIZE);
}

static void apply_func3(struct values_s *vals, unsigned int args, enum func_e func) {
    struct value_s new_value;
    double val1, val2;
    struct values_s *v = &vals[2];
    if (args != 2) err_msg2(ERROR_ILLEGAL_OPERA,NULL, &vals->epoint);
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

static void function_hypot(struct values_s *vals, unsigned int args) {
    return apply_func3(vals, args, F_HYPOT);
}

static void function_atan2(struct values_s *vals, unsigned int args) {
    return apply_func3(vals, args, F_ATAN2);
}

static void function_pow(struct values_s *vals, unsigned int args) {
    return apply_func3(vals, args, F_POW);
}

struct builtin_functions_s builtin_functions[] = {
    {"abs", function_abs}, 
    {"acos", function_acos}, 
    {"all", function_all},
    {"any", function_any},
    {"asin", function_asin}, 
    {"atan", function_atan}, 
    {"atan2", function_atan2}, 
    {"bool", function_bool}, 
    {"cbrt", function_cbrt}, 
    {"ceil", function_ceil},
    {"cos", function_cos}, 
    {"cosh", function_cosh}, 
    {"deg", function_deg}, 
    {"exp", function_exp}, 
    {"float", function_float}, 
    {"floor", function_floor},
    {"frac", function_frac}, 
    {"hypot", function_hypot}, 
    {"int", function_int},
    {"len", function_len},
    {"log", function_log},
    {"log10", function_log10}, 
    {"pow", function_pow}, 
    {"rad", function_rad}, 
    {"range", function_range},
    {"register", function_register},
    {"repr", function_repr},
    {"round", function_round},
    {"sign", function_sign}, 
    {"sin", function_sin}, 
    {"sinh", function_sinh}, 
    {"size", function_size},
    {"sqrt", function_sqrt}, 
    {"str", function_str},
    {"tan", function_tan}, 
    {"tanh", function_tanh}, 
    {"trunc", function_trunc}, 
    {NULL, NULL}
};

void functionobj_init(void) {
    obj_init(&obj, T_FUNCTION, "<function>");
    obj.hash = hash;
    obj.same = same;
    obj.repr = repr;
}
