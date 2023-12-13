/*
    $Id: intobj.c 3123 2023-09-16 10:50:10Z soci $

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#include "intobj.h"
#include <string.h>
#include "math.h"
#include "unicode.h"
#include "error.h"
#include "eval.h"
#include "variables.h"
#include "arguments.h"

#include "boolobj.h"
#include "floatobj.h"
#include "codeobj.h"
#include "strobj.h"
#include "bytesobj.h"
#include "bitsobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "errorobj.h"
#include "addressobj.h"
#include "functionobj.h"
#include "encobj.h"

#define SHIFT (8 * (unsigned int)sizeof(digit_t))
#define MASK (~(digit_t)0)
#define DSHIFT 9
#define DMUL ((digit_t)1000000000)

static Type obj;

static Int minus1_val = { { &obj, 1 }, -1, {1, 0}, minus1_val.val };
static Int zero_val = { { &obj, 1 }, 0, {0, 0}, zero_val.val };
static Int one_val = { { &obj, 1 }, 1, {1, 0}, one_val.val };

Type *const INT_OBJ = &obj;
Obj *const int_value[2] = { &zero_val.v, &one_val.v };
Obj *const minus1_value = &minus1_val.v;

static inline Int *ref_int(Int *v1) {
    v1->v.refcount++; return v1;
}

static inline size_t intlen(const Int *v1) {
    ssize_t len = v1->len;
    return (size_t)(len < 0 ? -len : len);
}

MUST_CHECK Obj *int_from_obj(Obj *v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_INT: return val_reference(v1);
    case T_FLOAT: return int_from_float(Float(v1), epoint);
    case T_CODE: return int_from_code(Code(v1), epoint);
    case T_STR: return int_from_str(Str(v1), epoint);
    case T_BOOL: return int_from_bool(Bool(v1));
    case T_BYTES: return int_from_bytes(Bytes(v1), epoint);
    case T_BITS: return int_from_bits(Bits(v1), epoint);
    case T_ADDRESS: return int_from_address(Address(v1), epoint);
    default: break;
    }
    return new_error_conv(v1, INT_OBJ, epoint);
}

static MUST_CHECK Obj *convert(oper_t op) {
    return int_from_obj(op->v2, op->epoint2);
}

static FAST_CALL NO_INLINE void int_destroy(Int *v1) {
    free(v1->data);
}

static FAST_CALL void destroy(Obj *o1) {
    Int *v1 = Int(o1);
    if unlikely(v1->val != v1->data) int_destroy(v1);
}

static inline MALLOC Int *new_int(void) {
    return Int(val_alloc(INT_OBJ));
}

static digit_t *inew(Int *v, size_t len) {
    digit_t *d;
    if (len <= lenof(v->val))  return v->val;
    new_array(&d, len);
    return d;
}

static digit_t *inew2(Int *v, size_t len) {
    if (len <= lenof(v->val))  return v->val;
    return allocate_array(digit_t, len);
}

static MUST_CHECK Obj *negate(Int *v1, linepos_t epoint) {
    Int *v;
    size_t ln;
    if (v1->len == 0) return val_reference(int_value[0]);
    v = new_int();
    ln = intlen(v1);
    v->data = inew2(v, ln);
    if (v->data == NULL) goto failed2;
    v->len = -v1->len;
    memcpy(v->data, v1->data, ln * sizeof *v->data);
    return Obj(v);
failed2:
    val_destroy(Obj(v));
    return new_error_mem(epoint);
}

static FAST_CALL NO_INLINE Obj *zeroint(Int *v) {
    val_destroy(Obj(v));
    return val_reference(int_value[0]);
}

static FAST_CALL NO_INLINE Obj *normalize2(Int *v, size_t sz) {
    do {
        sz--;
        v->val[sz] = v->data[sz];
    } while (sz != 0);
    free(v->data);
    v->data = v->val;
    return Obj(v);
}

static FAST_CALL MUST_CHECK Obj *normalize(Int *v, size_t sz, bool neg) {
    digit_t *d = v->data;
    while (sz != 0 && d[sz - 1] == 0) sz--;
    if (sz == 0) return zeroint(v);
    /*if (sz > SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    v->len = (ssize_t)(neg ? -sz : sz);
    if (v->val != d && sz <= lenof(v->val)) {
        return normalize2(v, sz);
    }
    return Obj(v);
}

static MUST_CHECK Obj *return_int(digit_t c, bool neg) {
    Int *vv;
    if (c < lenof(int_value) && (!neg || c == 0)) return val_reference(int_value[c]);
    vv = new_int();
    vv->data = vv->val;
    vv->val[0] = c;
    vv->len = neg ? -1 : 1;
    return Obj(vv);
}

static MUST_CHECK Obj *return_int_inplace(Int *vv, digit_t c, bool neg) {
    if (vv->data != vv->val) {
        int_destroy(vv);
        vv->data = vv->val;
    }
    vv->val[0] = c;
    vv->len = c == 0 ? 0 : neg ? -1 : 1;
    return val_reference(Obj(vv));
}

static void iadd(const Int *, const Int *, Int *);
static void isub(const Int *, const Int *, Int *);

static MUST_CHECK Obj *invert(Int *v1) {
    Int *v = new_int();
    if (v1->len < 0) isub(v1, Int(int_value[1]), v);
    else {
        iadd(v1, Int(int_value[1]), v);
        v->len = -v->len;
    }
    return Obj(v);
}

static FAST_CALL NO_INLINE bool int_same(const Int *v1, const Int *v2) {
    return memcmp(v1->data, v2->data, intlen(v1) * sizeof *v1->data) == 0;
}

static FAST_CALL bool same(const Obj *o1, const Obj *o2) {
    const Int *v1 = Int(o1), *v2 = Int(o2);
    if (o1->obj != o2->obj || v1->len != v2->len) return false;
    switch (v1->len) {
    case 0: return true;
    case -1:
    case 1: return v1->data[0] == v2->data[0];
    default: return int_same(v1, v2);
    }
}

static MUST_CHECK Obj *truth(Obj *o1, Truth_types UNUSED(type), linepos_t UNUSED(epoint)) {
    return truth_reference(Int(o1)->len != 0);
}

static MUST_CHECK Obj *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Int *v1 = Int(o1);
    ssize_t l = v1->len;
    unsigned int h;

    switch (l) {
    case -1: *hs = (-v1->val[0]) & ((~0U) >> 1); return NULL;
    case 0: *hs = 0; return NULL;
    case 1: *hs = v1->val[0] & ((~0U) >> 1); return NULL;
    }
    h = 0;
    if (l > 0) {
        while ((l--) != 0) {
            h += v1->data[l];
        }
    } else {
        while ((l++) != 0) {
            h -= v1->data[-l];
        }
    }
    *hs = h & ((~0U) >> 1);
    return NULL;
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Int *v1 = Int(o1);
    size_t len = intlen(v1);
    bool neg = v1->len < 0;
    uint8_t *s;
    digit_t ten, r, *out;
    size_t slen, i, j, sz, len2;
    Int tmp;
    Str *v;

    if (len <= 1) {
        static const digit_t d[9] = {1000000000, 100000000, 10000000, 1000000, 100000, 10000, 1000, 100, 10};
        digit_t dec;
        i = 9;
        if (len != 0) {
            len = (v1->len < 0) ? 1U : 0U;
            dec = v1->val[0];
            for (; i > 0; i--) {
                if (dec < d[i - 1]) break;
            }
        } else dec = 0;
        len += 10 - i;
        if (len > maxsize) return NULL;
        v = new_str2(len);
        if (v == NULL) return NULL;
        v->chars = len;
        s = v->data;
        if (v1->len < 0) *s++ = '-';
        for (; i < 9; i++) {
            digit_t a = dec / d[i];
            dec = dec % d[i];
            *s++ = (uint8_t)('0' + a);
        }
        *s = (uint8_t)('0' + dec);
        return Obj(v);
    }

    sz = len * SHIFT / (3 * DSHIFT);
    if (len > SSIZE_MAX / SHIFT) return NULL; /* overflow */
    if (sz * DSHIFT > maxsize) return NULL;
    sz++;
    out = inew2(&tmp, sz);
    if (out == NULL) return NULL;

    for (sz = 0, i = len; (i--) != 0;) {
        digit_t h = v1->data[i];
        for (j = 0; j < sz; j++) {
            twodigits_t tm = ((twodigits_t)out[j] << SHIFT) | h;
            h = (digit_t)(tm / DMUL);
            out[j] = (digit_t)(tm - (twodigits_t)h * DMUL);
        }
        while (h != 0) {
            out[sz++] = h % DMUL;
            h /= DMUL;
        }
    }
    if (sz == 0) out[sz] = 0;
    else sz--;
    slen = neg ? 2 : 1;
    ten = 10;
    r = out[sz];
    while (r >= ten) {
        ten *= 10;
        slen++;
    }
    len2 = sz * DSHIFT;
    if (inc_overflow(&slen, len2) || sz > SIZE_MAX / DSHIFT) goto error; /* overflow */
    if (slen > maxsize) {
    error:
        if (tmp.val != out) free(out);
        return NULL;
    }
    v = new_str2(slen);
    if (v == NULL) goto error;
    v->chars = slen;
    s = v->data + slen;
    for (i = 0; i < sz; i++) {
        r = out[i];
        for (j = 0; j < DSHIFT; j++) {
            *--s = (uint8_t)('0' + (r % 10));
            r /= 10;
        }
    }
    r = out[i];
    do {
        *--s = (uint8_t)('0' + (r % 10));
        r /= 10;
    } while (r != 0);
    if (neg) *--s = '-';

    if (tmp.val != out) free(out);
    return Obj(v);
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Int *v1 = Int(o1);
    Error *v;
    digit_t d;
    switch (v1->len) {
    case 1: d = v1->data[0];
            *iv = (ival_t)d;
            if (bits <= SHIFT && (d >> (bits - 1)) != 0) break;
            return NULL;
    case 0: *iv = 0; return NULL;
    case -1: d = v1->data[0] - 1;
             *iv = ~(ival_t)d;
             if (bits <= SHIFT && (d >> (bits - 1)) != 0) break;
             return NULL;
    default: break;
    }
    v = new_error(ERROR_____CANT_IVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = val_reference(o1);
    return v;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Int *v1 = Int(o1);
    Error *v;
    switch (v1->len) {
    case 1: *uv = v1->data[0];
            if (bits < SHIFT && (*uv >> bits) != 0) break;
            return NULL;
    case 0: *uv = 0; return NULL;
    default: break;
    }
    if (v1->len < 0) return Error(new_error_obj(ERROR______NOT_UVAL, o1, epoint));
    v = new_error(ERROR_____CANT_UVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = val_reference(o1);
    return v;
}

MUST_CHECK Obj *float_from_int(const Int *v1, linepos_t epoint) {
    size_t i, len1;
    double d;
    switch (v1->len) {
    case -1: d = -(double)v1->data[0]; break;
    case 0: d = 0.0; break;
    case 1: d = v1->data[0]; break;
    default:
        len1 = intlen(v1); d = v1->data[0];
        for (i = 1; i < len1; i++) {
            d += ldexp(v1->data[i], (int)(i * SHIFT));
        }
        if (v1->len < 0) d = -d;
        return float_from_double(d, epoint);
    }
    return new_float(d);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Int *v1 = Int(o1);
    return val_reference(v1->len < 0 ? minus1_value : int_value[(v1->len > 0) ? 1 : 0]);
}

static MUST_CHECK Obj *function(oper_t op) {
    Int *v1 = Int(op->v2);
    if (v1->len >= 0 || Function(op->v1)->func != F_ABS) return Obj(ref_int(v1));
    if (op->inplace == Obj(v1)) {
        v1->len = -v1->len;
        return val_reference(Obj(v1));
    }
    return negate(v1, op->epoint2);
}

static inline unsigned int ldigit(const Int *v1) {
    ssize_t len = v1->len;
    if (len < 0) return ~v1->data[0] + 1U;
    return (len != 0) ? v1->data[0] : 0;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Int *v1 = Int(op->v1);
    switch (op->op) {
    case O_BANK:
    case O_HIGHER:
    case O_LOWER:
    case O_HWORD:
    case O_WORD:
    case O_BSWORD:
        return bits_calc1(op->op, ldigit(v1));
    case O_INV:
        switch (v1->len) {
        case 1:
            if (~v1->data[0] == 0) break;
            if (op->inplace == Obj(v1)) {
                v1->data[0]++; v1->len = -1;
                return Obj(ref_int(v1));
            }
            return return_int(v1->data[0] + 1, true);
        case 0:
            return val_reference(minus1_value);
        case -1:
            if (v1->data[0] == 1) return val_reference(int_value[0]);
            if (op->inplace == Obj(v1)) {
                v1->data[0]--; v1->len = 1;
                return Obj(ref_int(v1));
            }
            return return_int(v1->data[0] - 1, false);
        default:
            break;
        }
        return invert(v1);
    case O_NEG:
        if (op->inplace != Obj(v1)) return negate(v1, op->epoint3);
        v1->len = -v1->len;
        return val_reference(Obj(v1));
    case O_POS: return Obj(ref_int(v1));
    case O_STRING:
        {
            Obj *o = repr(Obj(v1), op->epoint, SIZE_MAX);
            return (o != NULL) ? o : new_error_mem(op->epoint3);
        }
    case O_LNOT:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return truth_reference(v1->len == 0);
    default: break;
    }
    return obj_oper_error(op);
}

static void iadd(const Int *vv1, const Int *vv2, Int *vv) {
    size_t i, len1, len2;
    digit_t *v1, *v2, *v;
    digit_t d;
    digit_t c;
    len1 = intlen(vv1);
    len2 = intlen(vv2);
    if (len1 <= 1 && len2 <= 1) {
        v = vv->data = vv->val;
        d = vv1->val[0];
        v[0] = d + vv2->val[0];
        if (v[0] < d) {
            v[1] = 1;
            vv->len = 2;
            return;
        }
        vv->len = (v[0] != 0) ? 1 : 0;
        return;
    }
    if (len1 < len2) {
        const Int *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    if (len1 + 1 < 1) err_msg_out_of_memory(); /* overflow */
    v = inew(vv, len1 + 1);
    v1 = vv1->data; v2 = vv2->data;
    for (c = 0, i = 0; i < len2; i++) {
        d = v1[i];
        v[i] = d + v2[i] + c;
        c = ((c != 0) ? (v[i] <= d) : (v[i] < d)) ? 1 : 0;
    }
    for (;i < len1; i++) {
        v[i] = v1[i] + c;
        c = (v[i] < c) ? 1 : 0;
    }
    if (c != 0) v[i++] = 1;
    while (i != 0 && v[i - 1] == 0) i--;
    if (v != vv->val && i <= lenof(vv->val)) {
        memcpy(vv->val, v, i * sizeof *v);
        free(v);
        v = vv->val;
    }
    if (vv == vv1 || vv == vv2) destroy(Obj(vv));
    vv->data = v;
    vv->len = (ssize_t)i;
}

static void isub(const Int *vv1, const Int *vv2, Int *vv) {
    size_t i, len1, len2;
    digit_t *v1, *v2, *v;
    bool c;
    bool neg;
    len1 = intlen(vv1);
    len2 = intlen(vv2);
    if (len1 <= 1 && len2 <= 1) {
        digit_t d1 = vv1->val[0], d2 = vv2->val[0];
        v = vv->val;
        vv->data = v;
        if (d1 < d2) {
            v[0] = d2 - d1;
            vv->len = -1;
            return;
        }
        v[0] = d1 - d2;
        vv->len = (v[0] != 0) ? 1 : 0;
        return;
    }
    if (len1 < len2) {
        const Int *tmp = vv1; vv1 = vv2; vv2 = tmp;
        neg = true;
        i = len1; len1 = len2; len2 = i;
    } else {
        neg = false;
        if (len1 == len2) {
            i = len1;
            v1 = vv1->data; v2 = vv2->data;
            while (i != 0 && v1[i - 1] == v2[i - 1]) i--;
            if (i == 0) {
                if (vv == vv1 || vv == vv2) destroy(Obj(vv));
                vv->len = 0;
                vv->val[0] = 0;
                vv->data = vv->val;
                return;
            }
            if (v1[i - 1] < v2[i - 1]) {
                const Int *tmp = vv1; vv1 = vv2; vv2 = tmp;
                neg = true;
            }
            len1 = len2 = i;
        }
    }
    v = inew(vv, len1);
    v1 = vv1->data; v2 = vv2->data;
    for (c = false, i = 0; i < len2; i++) {
        if (c) {
            c = (v1[i] <= v2[i]);
            v[i] = v1[i] - v2[i] - 1;
            continue;
        }
        c = (v1[i] < v2[i]);
        v[i] = v1[i] - v2[i];
    }
    for (;c && i < len1; i++) {
        c = (v1[i] == 0);
        v[i] = v1[i] - 1;
    }
    for (;i < len1; i++) v[i] = v1[i];
    while (i != 0 && v[i - 1] == 0) i--;
    if (v != vv->val && i <= lenof(vv->val)) {
        memcpy(vv->val, v, i * sizeof *v);
        free(v);
        v = vv->val;
    }
    if (vv == vv1 || vv == vv2) destroy(Obj(vv));
    vv->data = v;
    vv->len = (ssize_t)(neg ? -i : i);
}

static void imul(const Int *vv1, const Int *vv2, Int *vv) {
    size_t i, j, len1, len2, sz;
    digit_t *v1, *v2, *v;
    Int tmp;
    len1 = intlen(vv1);
    len2 = intlen(vv2);
    if (add_overflow(len1, len2, &sz)) err_msg_out_of_memory();
    if (sz <= 2) {
        twodigits_t c = (twodigits_t)vv1->val[0] * vv2->val[0];
        v = vv->val;
        vv->data = v;
        if (c > (twodigits_t)MASK) {
            v[0] = (digit_t)c;
            v[1] = (digit_t)(c >> SHIFT);
            vv->len = 2;
            return;
        }
        v[0] = (digit_t)c;
        vv->len = (v[0] != 0) ? 1 : 0;
        return;
    }
    v = inew(&tmp, sz);
    memset(v, 0, sz * sizeof *v);
    v1 = vv1->data; v2 = vv2->data;
    for (i = 0; i < len1; i++) {
        twodigits_t c = 0, t = v1[i];
        digit_t *o = v + i;
        for (j = 0; j < len2; j++) {
            c += o[j] + v2[j] * t;
            o[j] = (digit_t)c;
            c >>= SHIFT;
        }
        if (c != 0) o[j] += (digit_t)c;
    }
    i = sz;
    while (i != 0 && v[i - 1] == 0) i--;
    if (vv == vv1 || vv == vv2) destroy(Obj(vv));
    if (i <= lenof(vv->val)) {
        if (i != 0) memcpy(vv->val, v, i * sizeof *v); else vv->val[0] = 0;
        if (tmp.val != v) free(v);
        v = vv->val;
    }
    vv->data = v;
    vv->len = (ssize_t)i;
}

static MUST_CHECK Obj *idivrem(oper_t op, bool divrem) {
    Int *vv1 = Int(op->v1), *vv2 = Int(op->v2);
    size_t len1, len2;
    bool neg, negr;
    digit_t *v1, *v2, *v;
    Int *vv;

    len2 = intlen(vv2);
    if (len2 == 0) {
        return new_error_obj(ERROR_DIVISION_BY_Z, op->v2, op->epoint2);
    }
    len1 = intlen(vv1);
    v1 = vv1->data;
    v2 = vv2->data;
    negr = (vv1->len < 0);
    neg = (negr != (vv2->len < 0));
    if (len1 < len2 || (len1 == len2 && v1[len1 - 1] < v2[len2 - 1])) {
        return val_reference(divrem ? ((neg && len1 != 0) ? minus1_value : int_value[0]) : Obj(vv1));
    }
    if (len2 == 1) {
        size_t i;
        twodigits_t r;
        digit_t n = v2[0];
        if (len1 == 1) {
            if (divrem) {
                digit_t t = v1[0] / n;
                if (neg && v1[0] != t * n) t++;
                return op->inplace == Obj(vv1) ? return_int_inplace(vv1, t, neg) : return_int(t, neg);
            }
            n = v1[0] % n;
            return op->inplace == Obj(vv1) ? return_int_inplace(vv1, n, negr) : return_int(n, negr);
        }
        r = 0;
        if (divrem) {
            vv = new_int();
            vv->data = v = inew(vv, len1);
            for (i = len1; (i--) != 0;) {
                digit_t h;
                r = (r << SHIFT) | v1[i];
                h = (digit_t)(r / n);
                v[i] = h;
                r -= (twodigits_t)h * n;
            }
            if (neg && r != 0) {
                for (i = 0; i < len1; i++) {
                    if ((v[i] = v[i] + 1) >= 1) break;
                }
            }
            return normalize(vv, len1, neg);
        }
        for (i = len1; (i--) != 0;) {
            digit_t h;
            r = (r << SHIFT) | v1[i];
            h = (digit_t)(r / n);
            r -= (twodigits_t)h * n;
        }
        return op->inplace == Obj(vv1) ? return_int_inplace(vv1, (digit_t)r, negr) : return_int((digit_t)r, negr);
    } else {
        size_t i, k;
        unsigned int d;
        digit_t wm1, wm2, *v0, *vk, *w0, *ak, *a;
        Int tmp1, tmp2, tmp3;

        if (len1 + 1 < 1) err_msg_out_of_memory(); /* overflow */
        v0 = inew(&tmp1, len1 + 1);
        w0 = inew(&tmp2, len2);

        d = 0;
        while ((v2[len2 - 1] << d) <= MASK / 2) d++;

        if (d != 0) {
            w0[0] = v2[0] << d;
            for (i = 1; i < len2; i++) w0[i] = (v2[i] << d) | (v2[i - 1] >> (SHIFT - d));
            v0[0] = v1[0] << d;
            for (i = 1; i < len1; i++) v0[i] = (v1[i] << d) | (v1[i - 1] >> (SHIFT - d));
            v0[i] = v1[i - 1] >> (SHIFT - d);
        } else {
            memcpy(w0, v2, len2 * sizeof *w0);
            v0[len1] = 0;
            memcpy(v0, v1, len1 * sizeof *v0);
        }
        if (v0[len1] != 0 || v0[len1 - 1] >= w0[len2 - 1]) len1++;

        k = len1 - len2;
        a = inew(&tmp3, k);

        wm1 = w0[len2 - 1]; wm2 = w0[len2 - 2];
        for (vk = v0 + k, ak = a + k; vk-- > v0;) {
            bool c = false;
            digit_t vtop = vk[len2];
            twodigits_t vvv = ((twodigits_t)vtop << SHIFT) | vk[len2 - 1];
            digit_t q = (digit_t)(vvv / wm1);
            digit_t r = (digit_t)(vvv - (twodigits_t)q * wm1);
            twodigits_t e;
            while ((twodigits_t)q * wm2 > (((twodigits_t)r << SHIFT) | vk[len2 - 2])) {
                --q;
                r += wm1;
                if (r < wm1) break;
            }
            for (e = i = 0; i < len2; i++) {
                digit_t t;
                e += (twodigits_t)q * w0[i];
                t = (digit_t)e; e >>= SHIFT;
                if (c) {
                    c = (vk[i] <= t);
                    vk[i] = vk[i] - t - 1;
                    continue;
                }
                c = vk[i] < t;
                vk[i] -= t;
            }
            if (c ? (vtop <= e) : (vtop < e)) {
                c = false;
                for (i = 0; i < len2; i++) {
                    digit_t t = vk[i];
                    if (c) {
                        c = ((vk[i] = t + w0[i] + 1) <= t);
                        continue;
                    }
                    c = ((vk[i] = t + w0[i]) < t);
                }
                --q;
            }
            *--ak = q;
        }
        if (w0 != tmp2.val) free(w0);

        vv = new_int();
        if (divrem) {
            if (neg) {
                while (len2 != 0 && v0[len2 - 1] == 0) len2--;
                if (len2 != 0) {
                    for (i = 0; i < k; i++) {
                        if ((a[i] = a[i] + 1) >= 1) break;
                    }
                }
            }
            if (v0 != tmp1.val) free(v0);
            if (a == tmp3.val) {
                memcpy(&vv->val, a, k * sizeof *a);
                a = vv->val;
            }
            vv->data = a;
            return normalize(vv, k, neg);
        }

        if (d != 0) {
            for (i = 0; i < len2 - 1; i++) v0[i] = (v0[i] >> d) | (v0[i + 1] << (SHIFT - d));
            v0[i] >>= d;
        }

        if (a != tmp3.val) free(a);
        if (v0 == tmp1.val) {
            memcpy(&vv->val, v0, len2 * sizeof *v0);
            v0 = vv->val;
        }
        vv->data = v0;
        return normalize(vv, len2, negr);
    }
}

static inline MUST_CHECK Obj *power(oper_t op) {
    Int *vv1 = Int(op->v1), *vv2 = Int(op->v2);
    digit_t d, j;
    size_t i;
    Int *v;

    i = (size_t)vv2->len;
    while (i != 0) {
        d = vv2->data[--i];
        for (j = (digit_t)1 << (SHIFT - 1); j != 0; j >>= 1) {
            if ((d & j) != 0) goto found;
        }
    }
    return val_reference(int_value[1]);
found:
    v = new_int();
    v->len = intlen(vv1);
    v->data = inew2(v, v->len);
    if (v->data == NULL) goto failed2;
    memcpy(v->data, vv1->data, v->len * sizeof *v->data);
    for (j >>= 1; j != 0; j >>= 1) {
        imul(v, v, v);
        if ((d & j) != 0) imul(v, vv1, v);
    }
    while (i != 0) {
        d = vv2->data[--i];
        for (j = (digit_t)1 << (SHIFT - 1); j != 0; j >>= 1) {
            imul(v, v, v);
            if ((d & j) != 0) imul(v, vv1, v);
        }
    }
    if (vv1->len < 0 && (vv2->data[0] & 1) != 0) v->len = -v->len;
    return Obj(v);
failed2:
    val_destroy(Obj(v));
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *lshift(oper_t op, uval_t s) {
    Int *vv1 = Int(op->v1);
    size_t i, len1, sz;
    unsigned int word, bit;
    digit_t *v1, *v, *v2;
    Int *vv;

    if (vv1->len == 0) return val_reference(int_value[0]);
    sz = word = s / SHIFT;
    bit = s % SHIFT;
    if (bit != 0) sz++;
    len1 = intlen(vv1);
    if (inc_overflow(&sz, len1)) goto failed;
    if (op->inplace == Obj(vv1) && sz <= lenof(vv->val)) {
        vv = ref_int(vv1);
        v = vv->data;
    } else {
        vv = new_int();
        vv->data = v = inew2(vv, sz);
        if (v == NULL) goto failed2;
    }
    v1 = vv1->data;
    v2 = v + word;
    if (bit != 0) {
        digit_t d = 0;
        for (i = len1; i != 0;) {
            digit_t d2 = v1[--i];
            v2[i + 1] = d | (d2 >> (SHIFT - bit));
            d = d2 << bit;
        }
        v2[0] = d;
    } else if (len1 != 0) memmove(v2, v1, len1 * sizeof *v2);
    if (word != 0) memset(v, 0, word * sizeof *v);

    return normalize(vv, sz, vv1->len < 0);
failed2:
    val_destroy(Obj(vv));
failed:
    return new_error_mem(op->epoint3);
}

static MUST_CHECK Obj *rshift(oper_t op, uval_t s) {
    Int *vv1 = Int(op->v1);
    size_t i, sz;
    unsigned int word, bit;
    digit_t *v1, *v;
    Int *vv;

    switch (vv1->len) {
    case 1:
        if (s < SHIFT) {
            digit_t d = vv1->val[0] >> s;
            return op->inplace == Obj(vv1) ? return_int_inplace(vv1, d, false) : return_int(d, false);
        }
        FALL_THROUGH; /* fall through */
    case 0:
        return val_reference(int_value[0]);
    case -1:
        if (s < SHIFT) {
            digit_t d = ((vv1->val[0] - 1) >> s) + 1;
            return op->inplace == Obj(vv1) ? return_int_inplace(vv1, d, true) : return_int(d, true);
        }
        return val_reference(minus1_value);
    default:
        break;
    }

    word = s / SHIFT;
    bit = s % SHIFT;
    sz = intlen(vv1);
    if (sz <= word) return val_reference(vv1->len < 0 ? minus1_value : int_value[0]);
    sz -= word;
    if (op->inplace == Obj(vv1)) {
        vv = ref_int(vv1);
        v = vv->data;
    } else {
        vv = new_int();
        vv->data = v = inew2(vv, sz);
        if (v == NULL) goto failed2;
    }
    v1 = vv1->data + word;
    if (vv1->len < 0) {
        bool c;
        for (i = 0; i < word; i++) {
            if (vv1->data[i] != 0) break;
        }
        c = (i == word);
        if (bit != 0) {
            bool c2 = true;
            for (i = 0; i < sz - 1; i++) {
                if (c) {
                    c = (v1[i] == 0);
                    v[i] = (v1[i] - 1) >> bit;
                } else v[i] = v1[i] >> bit;
                if (c) v[i] |= (v1[i + 1] - 1) << (SHIFT - bit);
                else v[i] |= v1[i + 1] << (SHIFT - bit);
                if (c2) {
                    v[i]++;
                    c2 = (v[i] == 0);
                }
            }
            if (c) v[i] = (v1[i] - 1) >> bit;
            else v[i] = v1[i] >> bit;
            if (c2) v[i]++;
        } else {
            if (c) {
                memmove(v, v1, sz * sizeof *v);
            } else {
                c = true;
                for (i = 0; c && i < sz; i++) {
                    v[i] = v1[i] + 1;
                    c = (v[i] == 0);
                }
                for (; i < sz; i++) v[i] = v1[i];
            }
        }
        return normalize(vv, sz, true);
    }
    if (bit != 0) {
        for (i = 0; i < sz - 1; i++) {
            v[i] = v1[i] >> bit;
            v[i] |= v1[i + 1] << (SHIFT - bit);
        }
        v[i] = v1[i] >> bit;
    } else memmove(v, v1, sz * sizeof *v);
    return normalize(vv, sz, false);
failed2:
    val_destroy(Obj(vv));
    return new_error_mem(op->epoint3);
}

static inline MUST_CHECK Obj *and_(oper_t op) {
    Int *vv1 = Int(op->v1), *vv2 = Int(op->v2);
    size_t i, len1, len2, sz;
    bool neg1, neg2;
    digit_t *v1, *v2, *v;
    Int *vv;
    len1 = intlen(vv1);
    len2 = intlen(vv2);

    if (len1 <= 1 && len2 <= 1) {
        digit_t c;
        neg1 = (vv1->len < 0); neg2 = (vv2->len < 0);
        c = neg1 ? -vv1->val[0] : vv1->val[0];
        c &= neg2 ? -vv2->val[0] : vv2->val[0];
        if (!neg2) neg1 = false;
        if (neg1) c = -c;
        return op->inplace == Obj(vv1) ? return_int_inplace(vv1, c, neg1) : return_int(c, neg1);
    }
    if (len1 < len2) {
        Int *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->data; v2 = vv2->data;
    neg1 = (vv1->len < 0); neg2 = (vv2->len < 0);

    sz = neg2 ? len1 : len2;
    if (neg1 && neg2) sz++;
    if (sz == 0) return val_reference(int_value[0]);
    vv = new_int();
    vv->data = v = inew2(vv, sz);
    if (v == NULL) goto failed2;

    if (neg1) {
        if (neg2) {
            bool c1 = true, c2 = true, c = true;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c1) {
                    c1 = (e == 0);
                    if (c2) {
                        c2 = (f == 0);
                        g = (~e + 1) & (~f + 1);
                    } else g = (~e + 1) & ~f;
                } else {
                    if (c2) {
                        c2 = (f == 0);
                        g = ~e & (~f + 1);
                    } else g = ~e & ~f;
                }
                if (c) {
                    c = (g == 0);
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            if (c2) {
                if (c) for (; i < len1; i++) v[i] = 0;
                else for (; i < len1; i++) v[i] = ~(digit_t)0;
            } else {
                for (; i < len1; i++) {
                    digit_t e = v1[i], g;
                    if (c1) {
                        c1 = (e == 0);
                        g = ~e + 1;
                    } else g = ~e;
                    if (c) {
                        c = (g == 0);
                        v[i] = ~g + 1;
                        continue;
                    }
                    v[i] = ~g;
                }
            }
            v[i] = c ? 1 : 0;
        } else {
            bool c1 = true;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i];
                if (c1) {
                    c1 = (e == 0);
                    v[i] = (~e + 1) & f;
                    continue;
                }
                v[i] = ~e & f;
            }
        }
    } else {
        if (neg2) {
            bool c2 = true;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i];
                if (c2) {
                    c2 = f == 0;
                    v[i] = e & (~f + 1);
                    continue;
                }
                v[i] = e & ~f;
            }
            if (c2) for (; i < len1; i++) v[i] = 0;
            else for (; i < len1; i++) v[i] = v1[i];
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] & v2[i];
        }
    }
    return normalize(vv, sz, neg1 && neg2);
failed2:
    val_destroy(Obj(vv));
    return new_error_mem(op->epoint3);
}

static inline MUST_CHECK Obj *or_(oper_t op) {
    Int *vv1 = Int(op->v1), *vv2 = Int(op->v2);
    size_t i, len1, len2, sz;
    bool neg1, neg2;
    digit_t *v1, *v2, *v;
    Int *vv;
    len1 = intlen(vv1);
    len2 = intlen(vv2);

    if (len1 <= 1 && len2 <= 1) {
        digit_t c;
        neg1 = (vv1->len < 0); neg2 = (vv2->len < 0);
        c = neg1 ? -vv1->val[0] : vv1->val[0];
        c |= neg2 ? -vv2->val[0] : vv2->val[0];
        if (neg2) neg1 = true;
        if (neg1) c = -c;
        return op->inplace == Obj(vv1) ? return_int_inplace(vv1, c, neg1) : return_int(c, neg1);
    }
    if (len1 < len2) {
        Int *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->data; v2 = vv2->data;
    neg1 = (vv1->len < 0); neg2 = (vv2->len < 0);

    sz = neg2 ? len2 : len1;
    if (neg1 || neg2) sz++;
    vv = new_int();
    vv->data = v = inew2(vv, sz);
    if (v == NULL) goto failed2;

    if (neg1) {
        bool c = true;
        if (neg2) {
            bool c1 = true, c2 = true;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c1) {
                    c1 = e == 0;
                    if (c2) {
                        c2 = f == 0;
                        g = (~e + 1) | (~f + 1);
                    } else g = (~e + 1) | ~f;
                } else {
                    if (c2) {
                        c2 = f == 0;
                        g = ~e | (~f + 1);
                    } else g = ~e | ~f;
                }
                if (c) {
                    c = g == 0;
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
        } else {
            bool c1 = true;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c1) {
                    c1 = e == 0;
                    g = (~e + 1) | f;
                } else g = ~e | f;
                if (c) {
                    c = g == 0;
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            for (; i < len1; i++) {
                digit_t e = v1[i], g;
                if (c1) {
                    c1 = e == 0;
                    g = ~e + 1;
                } else g = ~e;
                if (c) {
                    c = g == 0;
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
        }
        v[i] = c ? 1 : 0;
    } else {
        if (neg2) {
            bool c2 = true, c = true;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c2) {
                    c2 = (f == 0);
                    g = e | (~f + 1);
                } else g = e | ~f;
                if (c) {
                    c = (g == 0);
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            v[i] = c ? 1 : 0;
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] | v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    }
    return normalize(vv, sz, neg1 || neg2);
failed2:
    val_destroy(Obj(vv));
    return new_error_mem(op->epoint3);
}

static inline MUST_CHECK Obj *xor_(oper_t op) {
    Int *vv1 = Int(op->v1), *vv2 = Int(op->v2);
    size_t i, len1, len2, sz;
    bool neg1, neg2;
    digit_t *v1, *v2, *v;
    Int *vv;
    len1 = intlen(vv1);
    len2 = intlen(vv2);

    if (len1 <= 1 && len2 <= 1) {
        digit_t c;
        neg1 = (vv1->len < 0); neg2 = (vv2->len < 0);
        c = neg1 ? -vv1->val[0] : vv1->val[0];
        c ^= neg2 ? -vv2->val[0] : vv2->val[0];
        if (neg2) neg1 = !neg1;
        if (neg1) c = -c;
        return op->inplace == Obj(vv1) ? return_int_inplace(vv1, c, neg1) : return_int(c, neg1);
    }
    if (len1 < len2) {
        Int *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->data; v2 = vv2->data;
    neg1 = (vv1->len < 0); neg2 = (vv2->len < 0);

    sz = (neg1 != neg2) ? (len1 + 1) : len1;
    vv = new_int();
    vv->data = v = inew2(vv, sz);
    if (v == NULL) goto failed2;

    if (neg1) {
        if (neg2) {
            bool c1 = true, c2 = true;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c1) {
                    c1 = e == 0;
                    if (c2) {
                        c2 = f == 0;
                        g = (~e + 1) ^ (~f + 1);
                    } else g = (~e + 1) ^ ~f;
                } else {
                    if (c2) {
                        c2 = f == 0;
                        g = ~e ^ (~f + 1);
                    } else g = e ^ f;
                }
                v[i] = g;
            }
            for (; i < len1; i++) {
                digit_t e = v1[i], g;
                if (c1) {
                    c1 = e == 0;
                    g = ~e + 1;
                } else g = ~e;
                v[i] = c2 ? g : ~g;
            }
        } else {
            bool c1 = true, c = true;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c1) {
                    c1 = (e == 0);
                    g = (~e + 1) ^ f;
                } else g = ~e ^ f;
                if (c) {
                    c = (g == 0);
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            for (; i < len1; i++) {
                digit_t e = v1[i], g;
                if (c1) {
                    c1 = (e == 0);
                    g = ~e + 1;
                } else g = ~e;
                if (c) {
                    c = (g == 0);
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            v[i] = c ? 1 : 0;
        }
    } else {
        if (neg2) {
            bool c2 = true, c = true;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c2) {
                    c2 = (f == 0);
                    g = e ^ (~f + 1);
                } else g = e ^ ~f;
                if (c) {
                    c = (g == 0);
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            for (; i < len1; i++) {
                digit_t e = v1[i], g;
                g = c2 ? e : ~e;
                if (c) {
                    c = (g == 0);
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            v[i] = c ? 1 : 0;
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    }
    return normalize(vv, sz, neg1 != neg2);
failed2:
    val_destroy(Obj(vv));
    return new_error_mem(op->epoint3);
}

static ssize_t icmp(oper_t op) {
    const Int *vv1 = Int(op->v1), *vv2 = Int(op->v2);
    ssize_t i;
    size_t j;
    digit_t a, b;
    i = vv1->len - vv2->len;
    if (i != 0) return i;
    j = intlen(vv1);
    while (j != 0) {
        j--;
        a = vv1->data[j]; b = vv2->data[j];
        if (a != b) return (a > b) ? vv1->len : -vv1->len;
    }
    return 0;
}

MUST_CHECK Obj *int_from_size(size_t i) {
    unsigned int j;
    Int *v;
    if (i < lenof(int_value)) return val_reference(int_value[i]);
    v = new_int();
    v->data = v->val;
    v->val[0] = (digit_t)i;
    for (j = 1; j < sizeof i / sizeof *v->data; j++) {
        i >>= 4 * sizeof *v->data;
        i >>= 4 * sizeof *v->data;
        if (i == 0) break;
        v->val[j] = (digit_t)i;
    }
    v->len = (ssize_t)j;
    return Obj(v);
}

MUST_CHECK Obj *int_from_uval(uval_t i) {
    return return_int(i, false);
}

MUST_CHECK Obj *int_from_ival(ival_t i) {
    Int *v;
    if (i == 0) return val_reference(int_value[0]);
    v = new_int();
    v->data = v->val;
    if (i > 0) {
        v->val[0] = (uval_t)i;
        v->len = 1;
    } else {
        v->val[0] = -(uval_t)i;
        v->len = -1;
    }
    return Obj(v);
}

MUST_CHECK Obj *int_from_float(const Float *v1, linepos_t epoint) {
    bool neg;
    unsigned int expo;
    double frac, f = v1->real;
    size_t sz;
    digit_t *d;
    Int *v;

    neg = (f < 0.0);
    if (neg) f = -f;

    if (f < (double)(~(digit_t)0) + 1.0) return return_int((digit_t)f, neg);

    frac = frexp(f, (int *)&expo);
    sz = (expo - 1) / SHIFT + 1;

    v = new_int();
    v->data = d = inew2(v, sz);
    if (d == NULL) goto failed2;
    v->len = (ssize_t)(neg ? -sz : sz);

    frac = ldexp(frac, (int)((expo - 1) % SHIFT + 1));

    while ((sz--) != 0) {
        digit_t dg = (digit_t)frac;
        d[sz] = dg;
        frac = ldexp(frac - (double)dg, SHIFT);
    }
    return Obj(v);
failed2:
    val_destroy(Obj(v));
    return new_error_mem(epoint);
}

MUST_CHECK Obj *int_from_bytes(const Bytes *v1, linepos_t epoint) {
    unsigned int bits;
    size_t i, j, sz, len1;
    digit_t *d, uv;
    Int *v;
    bool inv;

    switch (v1->len) {
    case 1: return return_int(v1->data[0], false);
    case 0: return val_reference(int_value[0]);
    case ~0: return val_reference(minus1_value);
    case ~1: return return_int(v1->data[0] + 1U, true);
    }

    inv = v1->len < 0;
    len1 = (size_t)(inv ? -v1->len : v1->len); /* it's - for the additional length  */
    sz = len1 / sizeof *d;
    if ((len1 % sizeof *d) != 0) sz++;

    v = new_int();
    v->data = d = inew2(v, sz);
    if (d == NULL) goto failed2;

    uv = 0; bits = 0; j = 0; i = 0;
    if (inv) {
        uint8_t c = 0xff;
        for (;c == 0xff && i < len1 - 1; i++) {
            c = v1->data[i];
            uv |= (digit_t)((uint8_t)(c + 1)) << bits;
            if (bits == SHIFT - 8) {
                d[j++] = uv;
                bits = uv = 0;
            } else bits += 8;
        }
        for (; i < len1 - 1; i++) {
            uv |= (digit_t)v1->data[i] << bits;
            if (bits == SHIFT - 8) {
                d[j++] = uv;
                bits = uv = 0;
            } else bits += 8;
        }
        if (c == 0xff) uv |= 1U << bits;
        d[j] = uv;
    } else {
        for (;i < len1; i++) {
            uv |= (digit_t)v1->data[i] << bits;
            if (bits == SHIFT - 8) {
                d[j++] = uv;
                bits = uv = 0;
            } else bits += 8;
        }
        if (bits != 0) d[j] = uv;
    }

    return normalize(v, sz, inv);
failed2:
    val_destroy(Obj(v));
    return new_error_mem(epoint);
}

MUST_CHECK Obj *int_from_bits(const Bits *v1, linepos_t epoint) {
    bool inv;
    size_t i, sz;
    digit_t *d;
    const bdigit_t *b;
    Int *v;

    switch (v1->len) {
    case 1: return return_int(v1->data[0], false);
    case 0: return val_reference(int_value[0]);
    case ~0: return val_reference(minus1_value);
    }

    inv = v1->len < 0;
    sz = (size_t)(inv ? -v1->len : v1->len); /* it's - for the additional length  */
    if (sz == 0 && inv) goto failed; /* overflow */
    v = new_int();
    v->data = d = inew2(v, sz);
    if (d == NULL) goto failed2;

    b = v1->data;
    if (inv) {
        bool c = true;
        for (i = 0; c && i < sz - 1; i++) {
            c = (d[i] = b[i] + 1) < 1;
        }
        for (; i < sz - 1; i++) {
            d[i] = b[i];
        }
        d[i] = c ? 1 : 0;
    } else memcpy(d, b, sz * sizeof *d);

    return normalize(v, sz, inv);
failed2:
    val_destroy(Obj(v));
failed:
    return new_error_mem(epoint);
}

MUST_CHECK Obj *int_from_str(Str *v1, linepos_t epoint) {
    struct encoder_s *encoder;
    int ch;
    Int *v;
    digit_t uv;
    unsigned int bits;
    size_t i, j, sz, osz;
    digit_t *d;

    if (actual_encoding->updating) {
        if (v1->chars == 1) {
            unichar_t ch2 = v1->data[0];
            if ((ch2 & 0x80) != 0) utf8in(v1->data, &ch2);
            return int_from_uval(ch2);
        }
        if (v1->chars != 0) return new_error_obj(ERROR__NOT_ONE_CHAR, Obj(v1), epoint);
        return Obj(new_error(ERROR__EMPTY_STRING, epoint));
    }

    i = v1->len;
    if (i == 0) {
        return val_reference(int_value[0]);
    }

    sz = i / sizeof *d;
    if ((i % sizeof *d) != 0) sz++;
    v = new_int();
    v->data = d = inew2(v, sz);
    if (d == NULL) goto failed2;

    uv = 0; bits = 0; j = 0;
    encoder = enc_string_init(actual_encoding, v1, epoint);
    while ((ch = enc_string(encoder)) != EOF) {
        uv |= (digit_t)(ch & 0xff) << bits;
        if (bits == SHIFT - 8) {
            if (j >= sz) {
                if (v->val == d) {
                    sz = 16 / sizeof *d;
                    d = allocate_array(digit_t, 16 / sizeof *d);
                    if (d == NULL) goto failed2;
                    v->data = d;
                    memcpy(d, v->val, j * sizeof *d);
                } else {
                    if (inc_overflow(&sz, 1024 / sizeof *d)) goto failed2;
                    d = reallocate_array(d, sz);
                    if (d == NULL) goto failed2;
                    v->data = d;
                }
            }
            d[j++] = uv;
            bits = uv = 0;
        } else bits += 8;
    }
    if (bits != 0) {
        if (j >= sz) {
            sz++;
            if (v->val == d) {
                d = allocate_array(digit_t, sz);
                if (d == NULL) goto failed2;
                v->data = d;
                memcpy(d, v->val, j * sizeof *d);
            } else {
                d = reallocate_array(d, sz);
                if (d == NULL) goto failed2;
                v->data = d;
            }
        }
        d[j] = uv;
        osz = j + 1;
    } else osz = j;

    while (osz != 0 && d[osz - 1] == 0) osz--;
    if (osz == 0) return zeroint(v);
    v->len = (ssize_t)osz;
    if (v->val != d) {
        if (osz <= lenof(v->val)) return normalize2(v, osz);
        if (osz < sz) {
            digit_t *d2 = reallocate_array(d, osz);
            v->data = (d2 != NULL) ? d2 : d;
        }
    }
    return Obj(v);
failed2:
    val_destroy(Obj(v));
    return new_error_mem(epoint);
}

MUST_CHECK Obj *int_from_decstr(const uint8_t *s, linecpos_t *ln, linecpos_t *ln2) {
    const uint8_t *end;
    linecpos_t i, k;
    size_t sz;
    digit_t *d, *end2, val;
    Int *v;

    i = k = val = 0;
    if (s[0] != '_') {
        for (;;k++) {
            uint8_t c = s[k] ^ 0x30;
            if (c < 10) {
                if (val <= ((~(digit_t)0)-9)/10) val = val * 10 + c;
                continue;
            }
            if (c != ('_' ^ 0x30)) break;
            i++;
        }
        while (k != 0 && s[k - 1] == '_') {
            k--;
            i--;
        }
    }
    *ln = k;
    i = k - i;
    *ln2 = i;
    if (val <= ((~(digit_t)0)-9)/10) {
        if (val >= lenof(int_value)) {
            v = new_int();
            v->data = v->val;
            v->val[0] = val;
            v->len = 1;
            return Obj(v);
        }
        return val_reference(int_value[val]);
    }
    sz = (size_t)((double)i * 0.11073093649624542178511177326072356663644313812255859375) + 1;

    v = new_int();
    v->data = d = inew2(v, sz);
    if (d == NULL) goto failed2;

    end = s + k;
    end2 = d;
    while (s < end) {
        digit_t *d2, mul;
        val = 0;
        for (mul = 1; mul < 1000000000 && s < end; s++) {
            uint8_t c = *s ^ 0x30;
            if (c < 10) {
                val = val * 10 + c;
                mul *= 10;
            }
        }
        d2 = d;
        while (d2 < end2) {
            twodigits_t a = (twodigits_t)*d2 * mul + val;
            *d2++ = (digit_t)a;
            val = (digit_t)(a >> SHIFT);
        }
        if (val != 0) {
            if (end2 >= &d[sz]) {
                sz++;
                if (sz > lenof(v->val)) {
                    if (d == v->val) {
                        d = allocate_array(digit_t, sz);
                        if (d == NULL) goto failed2;
                        v->data = d;
                        memcpy(d, v->val, sizeof v->val);
                    } else {
                        d = reallocate_array(d, sz);
                        if (d == NULL) goto failed2;
                        v->data = d;
                    }
                }
                end2 = d + sz - 1;
            }
            *end2++ = val;
        }
    }

    sz = (size_t)(end2 - d);
    return normalize(v, sz, false);
failed2:
    val_destroy(Obj(v));
    return NULL;
}

static MUST_CHECK Obj *calc2_int(oper_t op) {
    Int *v1 = Int(op->v1), *v2 = Int(op->v2), *v;
    Error *err;
    Obj *val;
    ival_t shift;
    ssize_t cmp;
    switch (op->op) {
    case O_CMP:
        cmp = icmp(op);
        return val_reference(cmp < 0 ? minus1_value : int_value[(cmp > 0) ? 1 : 0]);
    case O_EQ: return truth_reference(icmp(op) == 0);
    case O_NE: return truth_reference(icmp(op) != 0);
    case O_MIN:
    case O_LT: return truth_reference(icmp(op) < 0);
    case O_LE: return truth_reference(icmp(op) <= 0);
    case O_MAX:
    case O_GT: return truth_reference(icmp(op) > 0);
    case O_GE: return truth_reference(icmp(op) >= 0);
    case O_ADD:
        v = (op->inplace == Obj(v1)) ? ref_int(v1) : new_int();
        if (v1->len < 0) {
            if (v2->len < 0) {
                iadd(v1, v2, v);
                v->len = -v->len;
            } else isub(v2, v1, v);
        } else {
            if (v2->len < 0) isub(v1, v2, v);
            else iadd(v1, v2, v);
        }
        return Obj(v);
    case O_SUB:
        v = (op->inplace == Obj(v1)) ? ref_int(v1) : new_int();
        if (v1->len < 0) {
            if (v2->len < 0) isub(v1, v2, v);
            else iadd(v1, v2, v);
            v->len = -v->len;
        } else {
            if (v2->len < 0) iadd(v1, v2, v);
            else isub(v1, v2, v);
        }
        return Obj(v);
    case O_MUL:
        v = (op->inplace == Obj(v1)) ? ref_int(v1) : new_int();
        if ((v1->len ^ v2->len) < 0) {
            imul(v1, v2, v);
            v->len = -v->len;
        } else imul(v1, v2, v);
        return Obj(v);
    case O_DIV:
        return idivrem(op, true);
    case O_MOD:
        val = idivrem(op, false);
        if (val->obj != INT_OBJ) return val;
        v = Int(val);
        if (v->len !=0 && (v->len ^ v2->len) < 0) {
            Int *vv = new_int();
            if (v->len < 0) isub(v2, v, vv);
            else isub(v, v2, vv);
            val_destroy(Obj(v));
            return Obj(vv);
        }
        return Obj(v);
    case O_EXP:
        if (v2->len == 1) {
            if (v2->data[0] == 2) {
                v = (op->inplace == Obj(v1)) ? ref_int(v1) : new_int();
                imul(v1, v1, v);
                return Obj(v);
            }
            if (v2->data[0] == 1) {
                return val_reference(Obj(v1));
            }
        } else if (v2->len == 0) {
            return val_reference(int_value[1]);
        } else if (v2->len < 0) {
            Obj *vv1 = float_from_int(v1, op->epoint);
            Obj *vv2 = float_from_int(v2, op->epoint2);
            op->v1 = vv1;
            op->v2 = vv2;
            op->inplace = (vv1->refcount == 1) ? vv1 : NULL;
            val = vv1->obj->calc2(op);
            if (val->obj == ERROR_OBJ) {
                error_obj_update(Error(val), vv1, Obj(v1));
                error_obj_update(Error(val), vv2, Obj(v2));
            }
            val_destroy(vv1);
            val_destroy(vv2);
            return val;
        }
        return power(op);
    case O_LSHIFT:
        err = ival(Obj(v2), &shift, 8 * sizeof shift, op->epoint2);
        if (err != NULL) return Obj(err);
        if (shift == 0) return val_reference(Obj(v1));
        return (shift < 0) ? rshift(op, -(uval_t)shift) : lshift(op, (uval_t)shift);
    case O_RSHIFT:
        err = ival(Obj(v2), &shift, 8 * sizeof shift, op->epoint2);
        if (err != NULL) return Obj(err);
        if (shift == 0) return val_reference(Obj(v1));
        return (shift < 0) ? lshift(op, -(uval_t)shift) : rshift(op, (uval_t)shift);
    case O_AND: return and_(op);
    case O_OR: return or_(op);
    case O_XOR: return xor_(op);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Obj *tmp, *ret, *v2 = op->v2;

    if (op->op == O_LAND) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference((Int(op->v1)->len != 0) ? v2 : op->v1);
    }
    if (op->op == O_LOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference((Int(op->v1)->len != 0) ? op->v1 : v2);
    }
    if (op->op == O_LXOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return calc2_lxor(op, Int(op->v1)->len != 0);
    }
    switch (v2->obj->type) {
    case T_INT: return calc2_int(op);
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        tmp = int_value[Bool(v2)->value ? 1 : 0];
        op->v2 = tmp;
        ret = calc2_int(op);
        if (ret->obj == ERROR_OBJ) error_obj_update(Error(ret), tmp, v2);
        return ret;
    case T_BYTES:
        tmp = int_from_bytes(Bytes(v2), op->epoint2);
        goto conv;
    case T_BITS:
        tmp = int_from_bits(Bits(v2), op->epoint2);
        goto conv;
    case T_STR:
        tmp = int_from_str(Str(v2), op->epoint2);
    conv:
        op->v2 = tmp;
        if (op->inplace != NULL && op->inplace->refcount != 1) op->inplace = NULL;
        ret = calc2(op);
        if (ret->obj == ERROR_OBJ) error_obj_update(Error(ret), tmp, v2);
        val_destroy(tmp);
        return ret;
    default:
        if (op->op != O_MEMBER && op->op != O_X) {
            return v2->obj->rcalc2(op);
        }
        if (v2 == none_value || v2->obj == ERROR_OBJ) return val_reference(v2);
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Obj *tmp, *v1 = op->v1;
    if (v1->obj == BOOL_OBJ) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        switch (op->op) {
        case O_LSHIFT:
        case O_RSHIFT: tmp = bits_value[Bool(v1)->value ? 1 : 0]; break;
        default: tmp = int_value[Bool(v1)->value ? 1 : 0]; break;
        }
        op->v1 = tmp;
        op->inplace = NULL;
        return tmp->obj->calc2(op);
    }
    return obj_oper_error(op);
}

void intobj_init(void) {
    Type *type = new_type(&obj, T_INT, "int", sizeof(Int));
    type->convert = convert;
    type->destroy = destroy;
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
}

void intobj_names(void) {
    new_builtin("int", val_reference(Obj(INT_OBJ)));
}

void intobj_destroy(void) {
#ifdef DEBUG
    if (int_value[0]->refcount != 1) fprintf(stderr, "int[0] %" PRIuSIZE "\n", int_value[0]->refcount - 1);
    if (int_value[1]->refcount != 1) fprintf(stderr, "int[1] %" PRIuSIZE "\n", int_value[1]->refcount - 1);
    if (minus1_value->refcount != 1) fprintf(stderr, "int[-1] %" PRIuSIZE "\n", minus1_value->refcount - 1);
#endif
}
