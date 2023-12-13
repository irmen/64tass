/*
    $Id: functionobj.c 3121 2023-09-16 06:38:33Z soci $

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
#include "functionobj.h"
#include <string.h>
#include "math.h"
#include "isnprintf.h"
#include "eval.h"
#include "variables.h"
#include "error.h"
#include "file.h"
#include "arguments.h"
#include "instruction.h"
#include "64tass.h"
#include "section.h"

#include "floatobj.h"
#include "strobj.h"
#include "listobj.h"
#include "intobj.h"
#include "boolobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "bytesobj.h"
#include "dictobj.h"
#include "addressobj.h"

static Type obj;

Type *const FUNCTION_OBJ = &obj;

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Function *v1 = Function(o1), *v2 = Function(o2);
    return o1->obj == o2->obj && v1->func == v2->func;
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Function *v1 = Function(o1);
    int h = v1->name_hash;
    if (h < 0) {
        str_t s;
        s.data = (const uint8_t *)v1->name;
        s.len = v1->name_len;
        v1->name_hash = h = str_hash(&s);
    }
    *hs = h;
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t epoint, size_t maxsize) {
    const Function *v1 = Function(o1);
    uint8_t *s;
    size_t len;
    size_t name_len;
    Str *v;
    if (epoint == NULL) return NULL;
    name_len = v1->name_len;
    if (add_overflow(name_len, 20, &len)) return NULL;
    if (len > maxsize) return NULL;
    v = new_str2(len);
    if (v == NULL) return NULL;
    v->chars = len;
    s = v->data;
    memcpy(s, "<native_function '", 18);
    s += 18;
    memcpy(s, v1->name, name_len);
    s += name_len;
    *s = '\'';
    s[1] = '>';
    return Obj(v);
}

static MUST_CHECK Obj *str(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    const Function *v1 = Function(o1);
    Str *v;
    size_t name_len = v1->name_len;
    if (name_len > maxsize) return NULL;
    v = new_str2(name_len);
    if (v == NULL) return NULL;
    v->chars = name_len;
    memcpy(v->data, v1->name, name_len);
    return Obj(v);
}

static MUST_CHECK Obj *gen_broadcast(oper_t op, apply_func_t f) {
    Funcargs *vals = Funcargs(op->v2);
    struct values_s *v = vals->val;
    argcount_t args = vals->len;
    argcount_t j, k = 0;
    size_t ln = 1;
    List *vv;
    struct elements_s {
        Obj *oval;
        struct iter_s iter;
    };
    struct elements_s elements3[3], *elements = NULL;

    for (j = 0; j < args; j++) {
        const Type *objt = v[j].val->obj;
        if (objt->iterable) {
            struct iter_s *iter;
            if (elements == NULL) {
                if (args <= lenof(elements3)) {
                    elements = elements3;
                } else {
                    elements = allocate_array(struct elements_s, args);
                    if (elements == NULL) goto failed;
                }
                k = j;
            }
            iter = &elements[j].iter;
            elements[j].oval = iter->data = v[j].val; objt->getiter(iter);
            if (iter->len == 1) {
                v[j].val = iter->next(iter);
            } else if (iter->len != ln) {
                if (ln != 1) {
                    Error *err = new_error(ERROR_CANT_BROADCAS, &v[j].epoint);
                    err->u.broadcast.v1 = ln;
                    err->u.broadcast.v2 = iter->len;
                    for (; k < j + 1; k++) {
                        if (elements[k].oval == NULL) continue;
                        v[k].val = elements[k].oval;
                        iter_destroy(&elements[k].iter);
                    }
                    if (elements != elements3) free(elements);
                    return Obj(err);
                }
                ln = iter->len;
            }
        } else if (elements != NULL) {
            elements[j].oval = NULL;
        }
    }
    if (elements == NULL) {
        return f(op);
    }
    if (ln != 0) {
        size_t i;
        Obj **vals2;
        vv = List(val_alloc(v[k].val->obj == TUPLE_OBJ ? TUPLE_OBJ : LIST_OBJ));
        vals2 = vv->data = list_create_elements(vv, ln);
        for (i = 0; i < ln; i++) {
            for (j = k; j < args; j++) {
                if (elements[j].oval == NULL) continue;
                if (elements[j].iter.len != 1) v[j].val = elements[j].iter.next(&elements[j].iter);
            }
            vals2[i] = gen_broadcast(op, f);
        }
        vv->len = i;
    } else {
        vv = List(val_reference(v[k].val->obj == TUPLE_OBJ ? null_tuple : null_list));
    }
    for (; k < args; k++) {
        if (elements[k].oval == NULL) continue;
        v[k].val = elements[k].oval;
        iter_destroy(&elements[k].iter);
    }
    if (elements != elements3) free(elements);
    return Obj(vv);
failed:
    return new_error_mem(op->epoint);
}

/* range([start],end,[step]) */
static MUST_CHECK Obj *function_range(oper_t op) {
    Funcargs *vals = Funcargs(op->v2);
    struct values_s *v = vals->val;
    List *new_value;
    Error *err = NULL;
    ival_t start = 0, end, step = 1;
    size_t len2, i;
    Obj **val;

    switch (vals->len) {
    default: end = 0; break; /* impossible */
    case 1:
        err = v[0].val->obj->ival(v[0].val, &end, 8 * sizeof end, &v[0].epoint);
        break;
    case 3:
        err = v[2].val->obj->ival(v[2].val, &step, 8 * sizeof step, &v[2].epoint);
        if (err != NULL) return Obj(err);
        FALL_THROUGH; /* fall through */
    case 2:
        err = v[0].val->obj->ival(v[0].val, &start, 8 * sizeof start, &v[0].epoint);
        if (err != NULL) return Obj(err);
        err = v[1].val->obj->ival(v[1].val, &end, 8 * sizeof end, &v[1].epoint);
        break;
    }
    if (err != NULL) return Obj(err);
    if (step == 0) {
        return Obj(new_error(ERROR_NO_ZERO_VALUE, &v[2].epoint));
    }
    if (step > 0) {
        if (end < start) end = start;
        len2 = (uval_t)(end - start + step - 1) / (uval_t)step;
    } else {
        if (end > start) end = start;
        len2 = (uval_t)(start - end - step - 1) / -(uval_t)step;
    }
    new_value = new_list();
    val = list_create_elements(new_value, len2);
    for (i = 0; i < len2; i++) {
        val[i] = int_from_ival(start);
        start += step;
    }
    new_value->len = len2;
    new_value->data = val;
    return Obj(new_value);
}

static uint64_t state[2];

static uint64_t random64(void) {
    uint64_t a = state[0];
    const uint64_t b = state[1];
    state[0] = b;
    a ^= a << 23;
    a ^= a >> 17;
    a ^= b ^ (b >> 26);
    state[1] = a;
    return a + b;
}

void random_reseed(Obj *o1, linepos_t epoint) {
    Obj *v = int_from_obj(o1, epoint);
    if (v->obj != INT_OBJ) {
        if (v == none_value) err_msg_still_none(NULL, epoint);
        else if (v->obj == ERROR_OBJ) err_msg_output(Error(v));
    } else {
        Int *v1 = Int(v);

        state[0] = (((uint64_t)0x5229a30f) << 32) | (uint64_t)0x996ad7eb;
        state[1] = (((uint64_t)0xc03bbc75) << 32) | (uint64_t)0x3f671f6f;

        switch (v1->len) {
        case 4: state[1] ^= ((uint64_t)v1->data[3] << (8 * sizeof *v1->data)); FALL_THROUGH; /* fall through */
        case 3: state[1] ^= v1->data[2]; FALL_THROUGH; /* fall through */
        case 2: state[0] ^= ((uint64_t)v1->data[1] << (8 * sizeof *v1->data)); FALL_THROUGH; /* fall through */
        case 1: state[0] ^= v1->data[0]; FALL_THROUGH; /* fall through */
        case 0: break;
        default:
            if (v1->len < 0) {
                err_msg2(ERROR______NOT_UVAL, o1, epoint);
            } else {
                Error *err = new_error(ERROR_____CANT_UVAL, epoint);
                err->u.intconv.bits = 128;
                err->u.intconv.val = val_reference(o1);
                err_msg_output_and_destroy(err);
            }
        }
    }
    val_destroy(v);
}

/* random() */
static MUST_CHECK Obj *function_random(oper_t op) {
    Funcargs *vals = Funcargs(op->v2);
    struct values_s *v = vals->val;
    Error *err = NULL;
    ival_t start = 0, end, step = 1;
    uval_t len2;

    switch (vals->len) {
    default:
        return new_float((double)(int64_t)(random64() & (((uint64_t)1 << 53) - 1)) * ldexp(1, -53));
    case 1:
        err = v[0].val->obj->ival(v[0].val, &end, 8 * sizeof end, &v[0].epoint);
        break;
    case 3:
        err = v[2].val->obj->ival(v[2].val, &step, 8 * sizeof step, &v[2].epoint);
        if (err != NULL) return Obj(err);
        FALL_THROUGH; /* fall through */
    case 2:
        err = v[0].val->obj->ival(v[0].val, &start, 8 * sizeof start, &v[0].epoint);
        if (err != NULL) return Obj(err);
        err = v[1].val->obj->ival(v[1].val, &end, 8 * sizeof end, &v[1].epoint);
        break;
    }
    if (err != NULL) return Obj(err);
    if (step == 0) {
        return Obj(new_error(ERROR_NO_ZERO_VALUE, &v[2].epoint));
    }
    if (step > 0) {
        if (end < start) end = start;
        len2 = (uval_t)(end - start + step - 1) / (uval_t)step;
    } else {
        if (end > start) end = start;
        len2 = (uval_t)(start - end - step - 1) / -(uval_t)step;
    }
    if (len2 != 0) {
        if (step != 1 || (len2 & (len2 - 1)) != 0) {
            uval_t a = (~(uval_t)0) / len2;
            uval_t b = a * len2;
            uval_t r;
            do {
                r = (uval_t)random64();
            } while (r >= b);
            return int_from_ival(start + (ival_t)(r / a) * step);
        }
        return int_from_ival(start + (ival_t)(random64() & (len2 - 1)));
    }
    return Obj(new_error(ERROR___EMPTY_RANGE, op->epoint));
}

static struct oper_s sort_tmp;
static Obj *sort_error;
static Obj *sort_val;

static int sortcomp(void) {
    int ret;
    Obj *result;
    Obj *o1 = sort_tmp.v1;
    Obj *o2 = sort_tmp.v2;
    sort_tmp.inplace = NULL;
    result = sort_tmp.v1->obj->calc2(&sort_tmp);
    if (result->obj == INT_OBJ) ret = (int)Int(result)->len;
    else {
        ret = 0;
        if (sort_error == NULL) {
            if (result->obj == ERROR_OBJ) sort_error = val_reference(result);
            else {
                if (result->obj == TUPLE_OBJ || result->obj == LIST_OBJ) {
                    List *v1 = List(result);
                    size_t i;
                    for (i = 0; i < v1->len; i++) {
                        Obj *v2 = v1->data[i];
                        if (v2->obj == INT_OBJ) {
                            ret = (int)Int(v2)->len;
                            if (ret == 0) continue;
                            val_destroy(result);
                            return ret;
                        }
                        break;
                    }
                    if (i == v1->len) {
                        val_destroy(result);
                        return 0;
                    }
                }
                sort_tmp.v1 = o1;
                sort_tmp.v2 = o2;
                sort_error = obj_oper_error(&sort_tmp);
            }
        }
    }
    val_destroy(result);
    return ret;
}

static int list_sortcomp(const void *a, const void *b) {
    int ret;
    size_t aa = *(const size_t *)a, bb = *(const size_t *)b;
    List *list = List(sort_val);
    sort_tmp.v1 = list->data[aa];
    sort_tmp.v2 = list->data[bb];
    ret = sortcomp();
    if (ret == 0) return (aa > bb) ? 1 : -1;
    return ret;
}

static int dict_sortcomp(const void *a, const void *b) {
    int ret;
    size_t aa = *(const size_t *)a, bb = *(const size_t *)b;
    Dict *dict = Dict(sort_val);
    sort_tmp.v1 = dict->data[aa].key;
    sort_tmp.v2 = dict->data[bb].key;
    ret = sortcomp();
    if (ret == 0) return (aa > bb) ? 1 : -1;
    return ret;
}

/* sort() */
static MUST_CHECK Obj *function_sort(Obj *o1, linepos_t epoint) {
    if (o1->obj == TUPLE_OBJ || o1->obj == LIST_OBJ || o1->obj == DICT_OBJ) {
        size_t ln = (o1->obj == DICT_OBJ) ? Dict(o1)->len : List(o1)->len;
        if (ln > 1) {
            size_t i;
            Obj **vals;
            size_t *sort_index;
            sort_index = allocate_array(size_t, ln);
            if (sort_index == NULL) goto failed;
            for (i = 0; i < ln; i++) sort_index[i] = i;
            sort_val = o1;
            sort_error = NULL;
            sort_tmp.op = O_CMP;
            sort_tmp.epoint = sort_tmp.epoint2 = sort_tmp.epoint3 = epoint;
            qsort(sort_index, ln, sizeof *sort_index, (o1->obj == DICT_OBJ) ? dict_sortcomp : list_sortcomp);
            if (sort_error != NULL) {
                free(sort_index);
                return sort_error;
            }
            if (o1->obj == DICT_OBJ) o1 = dict_sort(Dict(o1), sort_index);
            else {
                List *v = List(val_alloc(o1->obj));
                v->data = vals = list_create_elements(v, ln);
                v->len = ln;
                for (i = 0; i < ln; i++) vals[i] = val_reference(List(o1)->data[sort_index[i]]);
                o1 = Obj(v);
            }
            free(sort_index);
            return o1;
        }
    }
    return val_reference(o1);
failed:
    return new_error_mem(epoint);
}

/* binary(name,[start],[length]) */
static MUST_CHECK Obj *function_binary(oper_t op) {
    Funcargs *vals = Funcargs(op->v2);
    struct values_s *v = vals->val;
    Error *err;
    ival_t offs = 0;
    uval_t length = (uval_t)-1;
    struct file_s *cfile2 = NULL;
    str_t filename;

    err = Error(tostr2(&v[0], &filename));
    if (err != NULL) return Obj(err);
    if (filename.len == 0) return Obj(new_error(ERROR__EMPTY_STRING, &v[0].epoint));
    cfile2 = file_open(&filename, current_file_list, FILE_OPEN_BINARY, &v[0].epoint);

    switch (vals->len) {
    case 3:
        err = v[2].val->obj->uval(v[2].val, &length, 8 * sizeof length, &v[2].epoint);
        if (err != NULL) return Obj(err);
        FALL_THROUGH; /* fall through */
    case 2:
        err = v[1].val->obj->ival(v[1].val, &offs, 8 * sizeof offs, &v[1].epoint);
        if (err != NULL) return Obj(err);
        FALL_THROUGH; /* fall through */
    default:
        break;
    }

    if (cfile2 != NULL) {
        filesize_t offset, ln = cfile2->binary.len;
        Bytes *b;
        if (offs < 0) offset = (-(uval_t)offs < ln) ? (ln - -(uval_t)offs) : 0;
        else offset = (uval_t)offs;
        if (offset < ln) ln -= offset; else ln = 0;
        if (length < ln) ln = length;
        if (ln == 0) return val_reference(null_bytes);
        if (((size_t)ln + 0) > SSIZE_MAX) return new_error_mem(op->epoint);
        b = new_bytes(ln);
        b->len = (ssize_t)ln;
        memcpy(b->data, cfile2->binary.data + offset, ln);
        return Obj(b);
    }
    return ref_none();
}

static Obj *function_unsigned_bytes(oper_t op, unsigned int bits) {
    uval_t uv;
    if (touval(op->v2, &uv, bits, op->epoint2)) uv = 0;
    return bytes_from_uval(uv, bits >> 3);
}

static Obj *function_signed_bytes(oper_t op, unsigned int bits) {
    ival_t iv;
    if (toival(op->v2, &iv, bits, op->epoint2)) iv = 0;
    return bytes_from_uval((uval_t)iv, bits >> 3);
}

static Obj *function_rta_addr(oper_t op, bool rta) {
    uval_t uv;
    Obj *val = op->v2;
    atype_t am = val->obj->address(val);
    if (touaddress(val, &uv, (am == A_KR) ? 16 : all_mem_bits, op->epoint2)) {
        uv = 0;
    } else {
        uv &= all_mem;
        switch (am) {
        case A_NONE:
            if ((current_address->l_address ^ uv) > 0xffff) err_msg2(ERROR_CANT_CROSS_BA, val, op->epoint2);
            break;
        case A_KR:
            break;
        default:
            err_msg_output_and_destroy(err_addressing(am, op->epoint2, -1));
        }
        if (rta) uv--;
    }
    return bytes_from_uval(uv, 2);
}

static MUST_CHECK Obj *function_function(oper_t op) {
    Obj *o2 = op->v2;
    const Type *typ = o2->obj;
    bool inplace = op->inplace == o2;
    double real;
    Error_types err;
    switch (Function(op->v1)->func) {
    case F_SIZE: return typ->size(op);
    case F_SIGN: return typ->sign(o2, op->epoint2);
    case F_BYTE: return function_unsigned_bytes(op, 8);
    case F_WORD: return function_unsigned_bytes(op, 16);
    case F_LONG: return function_unsigned_bytes(op, 24);
    case F_DWORD: return function_unsigned_bytes(op, 32);
    case F_CHAR: return function_signed_bytes(op, 8);
    case F_SINT: return function_signed_bytes(op, 16);
    case F_LINT: return function_signed_bytes(op, 24);
    case F_DINT: return function_signed_bytes(op, 32);
    case F_ADDR: return function_rta_addr(op, false);
    case F_RTA: return function_rta_addr(op, true);
    case F_CEIL:
    case F_FLOOR:
    case F_ROUND:
    case F_TRUNC:
    case F_ABS: return typ->function(op);
    case F_REPR:
        {
            Obj *v = typ->repr(o2, op->epoint2, SIZE_MAX);
            return v != NULL ? v : new_error_mem(op->epoint2);
        }
    default: break;
    }
    if (typ != FLOAT_OBJ) {
        o2 = float_from_obj(o2, op->epoint2);
        if (o2->obj != FLOAT_OBJ) return o2;
        inplace = o2->refcount == 1;
    }
    real = Float(o2)->real;
    switch (Function(op->v1)->func) {
    case F_SQRT:
        if (real < 0.0) {
            err = ERROR_SQUARE_ROOT_N; goto failed;
        }
        real = sqrt(real);
        break;
    case F_LOG10:
        if (real <= 0.0) {
            err = ERROR_LOG_NON_POSIT; goto failed;
        }
        real = log10(real);
        break;
    case F_LOG:
        if (real <= 0.0) {
            err = ERROR_LOG_NON_POSIT; goto failed;
        }
        real = log(real);
        break;
    case F_EXP: real = exp(real);break;
    case F_SIN: real = sin(real);break;
    case F_COS: real = cos(real);break;
    case F_TAN: real = tan(real);break;
    case F_ACOS:
        if (real < -1.0 || real > 1.0) {
            err = ERROR___MATH_DOMAIN; goto failed;
        }
        real = acos(real);
        break;
    case F_ASIN:
        if (real < -1.0 || real > 1.0) {
            err = ERROR___MATH_DOMAIN; goto failed;
        }
        real = asin(real);
        break;
    case F_ATAN: real = atan(real);break;
    case F_CBRT: real = cbrt(real);break;
    case F_FRAC: real -= trunc(real);break;
    case F_RAD: real = real * M_PI / 180.0;break;
    case F_DEG: real = real * 180.0 / M_PI;break;
    case F_COSH: real = cosh(real);break;
    case F_SINH: real = sinh(real);break;
    case F_TANH: real = tanh(real);break;
    default: real = HUGE_VAL; break; /* can't happen */
    }
    if (!inplace || real == HUGE_VAL || real == -HUGE_VAL || real != real) {
        if (typ != FLOAT_OBJ) val_destroy(o2);
        return float_from_double(real, op->epoint2);
    }
    Float(o2)->real = real;
    return typ != FLOAT_OBJ ? o2 : val_reference(o2);
failed:
    if (typ != FLOAT_OBJ) val_destroy(o2);
    return new_error_obj(err, op->v2, op->epoint2);
}

static MUST_CHECK Obj *apply_func(oper_t op, apply_func_t f) {
    Obj *o2 = op->v2;
    const Type *typ = o2->obj;

    if (typ->iterable) {
        bool inplace = op->inplace == o2;
        List *v;
        size_t i, len;
        Obj **vals;

        if (!inplace || (typ != TUPLE_OBJ && typ != LIST_OBJ)) {
            struct iter_s iter;
            iter.data = o2; typ->getiter(&iter);
            if (iter.len == 0) {
                iter_destroy(&iter);
                return val_reference(typ == TUPLE_OBJ ? null_tuple : null_list);
            }
            v = List(val_alloc(typ == TUPLE_OBJ ? TUPLE_OBJ : LIST_OBJ));
            v->data = vals = list_create_elements(v, iter.len);
            for (i = 0; i < iter.len && (o2 = iter.next(&iter)) != NULL; i++) {
                op->v2 = o2;
                op->inplace = inplace && o2->refcount == 1 ? o2 : NULL;
                vals[i] = apply_func(op, f);
            }
            iter_destroy(&iter);
            v->len = i;
            return Obj(v);
        }
        v = List(val_reference(o2));
        len = v->len;
        vals = v->data;
        for (i = 0; i < len; i++) {
            op->v2 = o2 = vals[i];
            op->inplace = inplace && o2->refcount == 1 ? o2 : NULL;
            vals[i] = apply_func(op, f);
            val_destroy(o2);
        }
        return Obj(v);
    }
    return f(op);
}

static MUST_CHECK Obj *to_real(struct values_s *v, double *r) {
    if (v->val->obj == FLOAT_OBJ) {
        *r = Float(v->val)->real;
    } else {
        Obj *val = float_from_obj(v->val, &v->epoint);
        if (val->obj != FLOAT_OBJ) return val;
        *r = Float(val)->real;
        val_destroy(val);
    }
    return NULL;
}

static MUST_CHECK Obj *function_hypot(oper_t op) {
    struct values_s *v = Funcargs(op->v2)->val;
    Obj *val;
    double real, real2;

    val = to_real(&v[0], &real);
    if (val != NULL) return val;
    val = to_real(&v[1], &real2);
    if (val != NULL) return val;
    return float_from_double(hypot(real, real2), op->epoint);
}

static MUST_CHECK Obj *function_atan2(oper_t op) {
    struct values_s *v = Funcargs(op->v2)->val;
    Obj *val;
    double real, real2;

    val = to_real(&v[0], &real);
    if (val != NULL) return val;
    val = to_real(&v[1], &real2);
    if (val != NULL) return val;
    return float_from_double(atan2(real, real2), op->epoint);
}

static MUST_CHECK Obj *function_pow(oper_t op) {
    struct values_s *v = Funcargs(op->v2)->val;
    op->op = O_EXP;
    op->epoint3 = op->epoint;
    op->epoint = &v[0].epoint;
    op->epoint2 = &v[1].epoint;
    op->v1 = v[0].val;
    op->v2 = v[1].val;
    return op->v1->obj->calc2(op);
}

static MUST_CHECK Obj *function_all_any(oper_t op, Obj **warn, bool first) {
    Truth_types typ = (Function(op->v1)->func == F_ALL) ? TRUTH_ALL : TRUTH_ANY;
    struct values_s *v = Funcargs(op->v2)->val;
    Obj *val = v->val;
    const Type *objt = val->obj;
    if (objt->iterable) {
        struct iter_s iter;
        Obj *good = (typ == TRUTH_ALL) ? true_value : false_value;
        Obj *result2 = val_reference(good);
        iter.data = val; objt->getiter(&iter);
        while ((v->val = iter.next(&iter)) != NULL) {
            Obj *result = v->val;
            if (result->obj == BOOL_OBJ) {
                if (result == good) continue;
                val_destroy(result2);
                result2 = val_reference(result);
                break;
            }
            result = function_all_any(op, warn, false);
            if (result == good) {
                val_destroy(result);
                continue;
            }
            val_destroy(result2); result2 = result;
            if (result->obj == BOOL_OBJ) {
                break;
            }
        }
        iter_destroy(&iter);
        v->val = val;
        return result2;
    }
    if (objt == BOOL_OBJ) {
        return val_reference(val);
    }
    if (!first) {
        if (diagnostics.strict_bool && *warn == NULL) *warn = val_reference(val);
        typ = TRUTH_BOOL;
    }
    return objt->truth(val, typ, &v->epoint);
}

static MUST_CHECK Obj *function_condition(oper_t op) {
    struct values_s *v = Funcargs(op->v2)->val;
    bool cond;

    if (v->val->obj == BOOL_OBJ) {
        cond = Bool(v->val)->value;
    } else {
        Obj *val = v->val->obj->truth(v->val, TRUTH_BOOL, &v->epoint);
        if (val->obj != BOOL_OBJ) return val;
        cond = Bool(val)->value;
        if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_BOOL, v->val, &v->epoint);
        val_destroy(val);
    }
    return val_reference(v[cond ? 1 : 2].val);
}

static inline int icmp(oper_t op) {
    Function_types v1 = Function(op->v1)->func;
    Function_types v2 = Function(op->v2)->func;
    if (v1 < v2) return -1;
    return (v1 > v2) ? 1 : 0;
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Function *v1 = Function(op->v1);
    Obj *o2 = op->v2;
    Function_types func;
    switch (o2->obj->type) {
    case T_FUNCTION:
        return obj_oper_compare(op, icmp(op));
    case T_FUNCARGS:
        {
            Funcargs *v2 = Funcargs(o2);
            argcount_t args = v2->len;
            struct values_s *v = v2->val;
            switch (op->op) {
            case O_FUNC:
                func = v1->func;
                switch (func) {
                case F_HYPOT:
                    if (args != 2) {
                        return new_error_argnum(args, 2, 2, op->epoint3);
                    }
                    return gen_broadcast(op, function_hypot);
                case F_ATAN2:
                    if (args != 2) {
                        return new_error_argnum(args, 2, 2, op->epoint3);
                    }
                    return gen_broadcast(op, function_atan2);
                case F_POW:
                    if (args != 2) {
                        return new_error_argnum(args, 2, 2, op->epoint3);
                    }
                    return function_pow(op);
                case F_RANGE:
                    if (args < 1 || args > 3) {
                        return new_error_argnum(args, 1, 3, op->epoint3);
                    }
                    return gen_broadcast(op, function_range);
                case F_BINARY:
                    if (args < 1 || args > 3) {
                        return new_error_argnum(args, 1, 3, op->epoint3);
                    }
                    return gen_broadcast(op, function_binary);
                case F_FORMAT:
                    if (args < 1) {
                        return new_error_argnum(args, 1, 0, op->epoint3);
                    }
                    return gen_broadcast(op, isnprintf);
                case F_RANDOM:
                    if (args > 3) {
                        return new_error_argnum(args, 0, 3, op->epoint3);
                    }
                    return gen_broadcast(op, function_random);
                default:
                    if (args != 1) {
                        return new_error_argnum(args, 1, 1, op->epoint3);
                    }
                    switch (func) {
                    case F_ANY:
                    case F_ALL:
                        {
                            Obj *warn = NULL;
                            Obj *ret = function_all_any(op, &warn, true);
                            if (warn != NULL) {
                                if (ret->obj == BOOL_OBJ) err_msg_bool(ERROR_____CANT_BOOL, warn, &v->epoint);
                                val_destroy(warn);
                            }
                            return ret;
                        }
                    case F_LEN:
                        op->v2 = v->val;
                        return v->val->obj->len(op);
                    case F_SORT:
                        return function_sort(v->val, &v->epoint);
                    default:
                        op->v2 = v->val;
                        op->inplace = v->val->refcount == 1 ? v->val : NULL;
                        return apply_func(op, function_function);
                    }
                }
            default: break;
            }
            break;
        }
    case T_NONE:
    case T_ERROR:
        return val_reference(o2);
    default:
        if (o2->obj->iterable && op->op != O_MEMBER && op->op != O_X) {
            return o2->obj->rcalc2(op);
        }
        break;
    }
    return obj_oper_error(op);
}

MUST_CHECK Obj *apply_function(oper_t op, apply_func_t f) {
    return apply_func(op, f);
}

MUST_CHECK Obj *apply_convert2(oper_t op) {
    return gen_broadcast(op, Type(op->v1)->convert2);
}

MUST_CHECK Obj *apply_condition(oper_t op) {
    return gen_broadcast(op, function_condition);
}

void functionobj_init(void) {
    Type *type = new_type(&obj, T_FUNCTION, "function", sizeof(Function));
    type->hash = hash;
    type->same = same;
    type->repr = repr;
    type->str = str;
    type->calc2 = calc2;
}

static Function builtin_functions[] = { /* maximum name length of 6 chars */
    { {NULL, 2}, "abs", 3, -1, F_ABS},
    { {NULL, 2}, "acos", 4, -1, F_ACOS},
    { {NULL, 2}, "addr", 4, -1, F_ADDR},
    { {NULL, 2}, "all", 3, -1, F_ALL},
    { {NULL, 2}, "any", 3, -1, F_ANY},
    { {NULL, 2}, "asin", 4, -1, F_ASIN},
    { {NULL, 2}, "atan", 4, -1, F_ATAN},
    { {NULL, 2}, "atan2", 5, -1, F_ATAN2},
    { {NULL, 2}, "binary", 6, -1, F_BINARY},
    { {NULL, 2}, "byte", 4, -1, F_BYTE},
    { {NULL, 2}, "cbrt", 4, -1, F_CBRT},
    { {NULL, 2}, "ceil", 4, -1, F_CEIL},
    { {NULL, 2}, "char", 4, -1, F_CHAR},
    { {NULL, 2}, "cos", 3, -1, F_COS},
    { {NULL, 2}, "cosh", 4, -1, F_COSH},
    { {NULL, 2}, "deg", 3, -1, F_DEG},
    { {NULL, 2}, "dint", 4, -1, F_DINT},
    { {NULL, 2}, "dword", 5, -1, F_DWORD},
    { {NULL, 2}, "exp", 3, -1, F_EXP},
    { {NULL, 2}, "floor", 5, -1, F_FLOOR},
    { {NULL, 2}, "format", 6, -1, F_FORMAT},
    { {NULL, 2}, "frac", 4, -1, F_FRAC},
    { {NULL, 2}, "hypot", 5, -1, F_HYPOT},
    { {NULL, 2}, "len", 3, -1, F_LEN},
    { {NULL, 2}, "lint", 4, -1, F_LINT},
    { {NULL, 2}, "log", 3, -1, F_LOG},
    { {NULL, 2}, "log10", 5, -1, F_LOG10},
    { {NULL, 2}, "long", 4, -1, F_LONG},
    { {NULL, 2}, "pow", 3, -1, F_POW},
    { {NULL, 2}, "rad", 3, -1, F_RAD},
    { {NULL, 2}, "random", 6, -1, F_RANDOM},
    { {NULL, 2}, "range", 5, -1, F_RANGE},
    { {NULL, 2}, "repr", 4, -1, F_REPR},
    { {NULL, 2}, "round", 5, -1, F_ROUND},
    { {NULL, 2}, "rta", 3, -1, F_RTA},
    { {NULL, 2}, "sign", 4, -1, F_SIGN},
    { {NULL, 2}, "sin", 3, -1, F_SIN},
    { {NULL, 2}, "sinh", 4, -1, F_SINH},
    { {NULL, 2}, "sint", 4, -1, F_SINT},
    { {NULL, 2}, "size", 4, -1, F_SIZE},
    { {NULL, 2}, "sort", 4, -1, F_SORT},
    { {NULL, 2}, "sqrt", 4, -1, F_SQRT},
    { {NULL, 2}, "tan", 3, -1, F_TAN},
    { {NULL, 2}, "tanh", 4, -1, F_TANH},
    { {NULL, 2}, "trunc", 5, -1, F_TRUNC},
    { {NULL, 2}, "word",  4, -1, F_WORD},
};

void functionobj_names(void) {
    unsigned int i;
    for (i = 0; i < lenof(builtin_functions); i++) {
        builtin_functions[i].v.obj = &obj;
        builtin_functions[i].v.refcount = 2;
        new_builtin(builtin_functions[i].name, Obj(builtin_functions + i));
    }
}

void functionobj_destroy(void) {
#ifdef DEBUG
    unsigned int i;
    for (i = 0; i < lenof(builtin_functions); i++) {
        if (builtin_functions[i].v.refcount != 1) {
            fprintf(stderr, "%s %" PRIuSIZE "\n", builtin_functions[i].name, builtin_functions[i].v.refcount - 1);
        }
    }
#endif
}
