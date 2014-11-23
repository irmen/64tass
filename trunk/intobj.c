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

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/
#include <string.h>
#include <math.h>
#include "values.h"
#include "intobj.h"
#include "unicode.h"
#include "encoding.h"

#include "boolobj.h"
#include "floatobj.h"

#define SHIFT (8 * sizeof(digit_t))
#define MASK (~(digit_t)0)
#define DSHIFT 9
#define DMUL ((digit_t)1000000000)

static struct obj_s obj;

obj_t INT_OBJ = &obj;

static inline size_t intlen(const value_t v1) {
    ssize_t len = v1->u.integer.len;
    return (len < 0) ? -len : len;
}

static MUST_CHECK value_t create(const value_t v1, linepos_t epoint) {
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_INT: return val_reference(v1);
    case T_FLOAT: return int_from_float(v1);
    case T_CODE: return int_from_code(v1, epoint);
    case T_STR: return int_from_str(v1, epoint);
    case T_BOOL: return int_from_bool(v1);
    case T_BYTES: return int_from_bytes(v1);
    case T_BITS: return int_from_bits(v1);
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return val_reference(none_value);
}

static void destroy(value_t v1) {
    if (v1->u.integer.val != v1->u.integer.data) free(v1->u.integer.data);
}

static digit_t *inew(value_t v, ssize_t len) {
    if (len > (ssize_t)sizeof(v->u.integer.val)/(ssize_t)sizeof(v->u.integer.val[0])) {
        digit_t *s = (digit_t *)malloc(len * sizeof(digit_t));
        if (!s || len > SSIZE_MAX / (ssize_t)sizeof(digit_t)) err_msg_out_of_memory(); /* overflow */
        return s; 
    }
    return v->u.integer.val;
}

static MUST_CHECK value_t negate(const value_t v1) {
    value_t v = val_alloc(INT_OBJ);
    v->u.integer.len = -v1->u.integer.len;
    if (v1->u.integer.len) {
        size_t ln = intlen(v1);
        v->u.integer.data = inew(v, ln);
        memcpy(v->u.integer.data, v1->u.integer.data, ln * sizeof(digit_t));
    } else {
        v->u.integer.data = v->u.integer.val;
        v->u.integer.val[0] = 0;
    }
    return v;
}

static MUST_CHECK value_t normalize(const value_t v, digit_t *d, size_t sz, int neg) {
    while (sz && !d[sz - 1]) sz--;
    if (v->u.integer.val != d && sz <= (ssize_t)sizeof(v->u.integer.val)/(ssize_t)sizeof(v->u.integer.val[0])) {
        memcpy(v->u.integer.val, d, sz * sizeof(digit_t));
        free(d);
        d = v->u.integer.val;
    }
    v->u.integer.data = d;
    v->u.integer.len = neg ? -sz : sz;
    return v;
}

static int same(const value_t v1, const value_t v2) {
    if (v2->obj != INT_OBJ || v1->u.integer.len != v2->u.integer.len) return 0;
    return !memcmp(v1->u.integer.data, v2->u.integer.data, intlen(v1) * sizeof(digit_t));
}

static MUST_CHECK value_t truth(const value_t v1, enum truth_e UNUSED(type), linepos_t UNUSED(epoint)) {
    return truth_reference(!!v1->u.integer.len);
}

static MUST_CHECK value_t hash(const value_t v1, int *hs, linepos_t UNUSED(epoint)) {
    ssize_t l = v1->u.integer.len;
    unsigned int h;
    
    switch (l) {
    case -1: *hs = (-v1->u.integer.val[0]) & ((~(unsigned int)0) >> 1); return NULL;
    case 0: *hs = 0; return NULL;
    case 1: *hs = v1->u.integer.val[0]; return NULL;
    }
    h = 0;
    if (l > 0) {
        while (l--) {
            h += v1->u.integer.val[l];
        }
    } else {
        while (l++) {
            h -= v1->u.integer.val[-l];
        }
    }
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t UNUSED(epoint)) {
    size_t len = intlen(v1);
    int neg = v1->u.integer.len < 0;
    uint8_t *s;
    digit_t ten, r, *out;
    size_t slen, i, j, sz, len2;
    struct value_s tmp;
    value_t v = val_alloc(STR_OBJ);

    if (len <= 1) {
        char tmp2[sizeof(digit_t) * 3];
        if (len) len = sprintf(tmp2, neg ? "-%u" : "%u", v1->u.integer.val[0]);
        else {tmp2[0]='0';len = 1;}
        s = str_create_elements(v, len);
        memcpy(s, tmp2, len);
        v->u.str.data = s;
        v->u.str.len = len;
        v->u.str.chars = len;
        return v;
    }

    sz = 1 + (len * SHIFT / (3 * DSHIFT));
    if (len > SSIZE_MAX / SHIFT) err_msg_out_of_memory(); /* overflow */
    out = inew(&tmp, sz);

    for (sz = 0, i = len; i--;) {
        digit_t h = v1->u.integer.data[i];
        for (j = 0; j < sz; j++) {
            twodigits_t tm = ((twodigits_t)out[j] << SHIFT) | h;
            h = (digit_t)(tm / DMUL);
            out[j] = (digit_t)(tm - (twodigits_t)h * DMUL);
        }
        while (h) {
            out[sz++] = h % DMUL;
            h /= DMUL;
        }
    }
    if (!sz) out[sz] = 0;
    else sz--;
    slen = 1 + neg;
    ten = 10;
    r = out[sz];
    while (r >= ten) {
        ten *= 10;
        slen++;
    }
    len2 = sz * DSHIFT;
    slen += len2;
    if (slen < len2 || sz > SIZE_MAX / DSHIFT) err_msg_out_of_memory(); /* overflow */
    s = str_create_elements(v, slen);
    s += slen;
    for (i = 0; i < sz; i++) {
        r = out[i];
        for (j = 0; j < DSHIFT; j++) {
            *--s = 0x30 | (r % 10);
            r /= 10;
        }
    }
    r = out[i];
    do {
        *--s = 0x30 | (r % 10);
        r /= 10;
    } while (r);
    if (neg) *--s = '-';

    if (tmp.u.integer.val != out) free(out);

    v->u.str.data = s;
    v->u.str.len = slen;
    v->u.str.chars = v->u.str.len;
    return v;
}

static MUST_CHECK value_t ival(const value_t v1, ival_t *iv, int bits, linepos_t epoint) {
    value_t v;
    switch (v1->u.integer.len) {
    case 1: *iv = v1->u.integer.data[0];
            if (bits < (int)(SHIFT-1) && (uval_t)*iv >> (bits-1)) break;
            return NULL;
    case 0: *iv = 0; return NULL;
    case -1: *iv = -v1->u.integer.data[0];
             if (bits < (int)(SHIFT-1) && (uval_t)~*iv >> (bits-1)) break;
             return NULL;
    default: break;
    }
    v = new_error_obj(ERROR_____CANT_IVAL, epoint);
    v->u.error.u.bits = bits;
    return v;
}

static MUST_CHECK value_t uval(const value_t v1, uval_t *uv, int bits, linepos_t epoint) {
    value_t v;
    switch (v1->u.integer.len) {
    case 1: *uv = v1->u.integer.data[0];
            if (bits < (int)SHIFT && *uv >> bits) break;
            return NULL;
    case 0: *uv = 0; return NULL;
    default: break;
    }
    v = new_error_obj(ERROR_____CANT_UVAL, epoint);
    v->u.error.u.bits = bits;
    return v;
}

MUST_CHECK value_t float_from_int(const value_t v1, linepos_t epoint) {
    size_t i, len1 = intlen(v1);
    double d = 0;
    for (i = 0; i < len1; i++) {
        if (v1->u.integer.len < 0) d -= ldexp((double)v1->u.integer.data[i], i * SHIFT);
        else d += ldexp((double)v1->u.integer.data[i], i * SHIFT);
        if (d == HUGE_VAL || d == -HUGE_VAL) {
            return new_error_obj(ERROR_NUMERIC_OVERF, epoint);
        }
    }
    return float_from_double(d);
}

static MUST_CHECK value_t sign(const value_t v1, linepos_t UNUSED(epoint)) {
    if (v1->u.integer.len < 0) return int_from_int(-1);
    return val_reference(int_value[v1->u.integer.len > 0]);
}

static MUST_CHECK value_t absolute(const value_t v1, linepos_t UNUSED(epoint)) {
    return (v1->u.integer.len >= 0) ? val_reference(v1) : negate(v1);
}

static void iadd(const value_t, const value_t, value_t);
static void isub(const value_t, const value_t, value_t);

static digit_t ldigit(const value_t v1) {
    ssize_t len = v1->u.integer.len;
    if (len < 0) return -v1->u.integer.data[0];
    return len ? v1->u.integer.data[0] : 0;
}

static MUST_CHECK value_t calc1(oper_t op) {
    value_t v1 = op->v1, v;
    digit_t uv;
    switch (op->op->u.oper.op) {
    case O_BANK: return bytes_from_u8(ldigit(v1) >> 16);
    case O_HIGHER: return bytes_from_u8(ldigit(v1) >> 8);
    case O_LOWER: return bytes_from_u8(ldigit(v1));
    case O_HWORD: return bytes_from_u16(ldigit(v1) >> 8);
    case O_WORD: return bytes_from_u16(ldigit(v1));
    case O_BSWORD:
        uv = ldigit(v1);
        return bytes_from_u16((uint8_t)(uv >> 8) | (uint16_t)(uv << 8));
    case O_INV:
        v = val_alloc(INT_OBJ);
        if (v1->u.integer.len < 0) isub(v1, int_value[1], v);
        else {
            iadd(v1, int_value[1], v);
            v->u.integer.len = -v->u.integer.len;
        }
        return v;
    case O_NEG: return negate(v1);
    case O_POS: return val_reference(v1);
    case O_STRING: return repr(v1, op->epoint);
    default: break;
    }
    return obj_oper_error(op);
}

static void iadd(value_t vv1, value_t vv2, value_t vv) {
    size_t i, len1, len2;
    digit_t *v1, *v2, *v;
    digit_t c;
    len1 = intlen(vv1);
    len2 = intlen(vv2);
    if (len1 <= 1 && len2 <= 1) {
        c = vv1->u.integer.val[0] + vv2->u.integer.val[0];
        v = vv->u.integer.val;
        vv->u.integer.data = v;
        if (c < vv1->u.integer.val[0]) {
            v[0] = c;
            v[1] = 1;
            vv->u.integer.len = 2;
            return;
        }
        v[0] = c;
        vv->u.integer.len = (v[0] != 0);
        return;
    }
    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    if (len1 + 1 < 1) err_msg_out_of_memory(); /* overflow */
    v = inew(vv, len1 + 1);
    v1 = vv1->u.integer.data; v2 = vv2->u.integer.data;
    for (c = i = 0; i < len2; i++) {
        if (c) {
            c = v1[i];
            c = (v[i] = c + v2[i] + 1) <= c;
            continue;
        }
        c = v1[i];
        c = (v[i] = c + v2[i]) < c;
    }
    for (;c && i < len1; i++) {
        c = (v[i] = v1[i] + 1) < 1;
    }
    for (;i < len1; i++) v[i] = v1[i];
    if (c) v[i++] = c;
    while (i && !v[i - 1]) i--;
    if (v != vv->u.integer.val && i <= sizeof(vv->u.integer.val)/sizeof(vv->u.integer.val[0])) {
        memcpy(vv->u.integer.val, v, i * sizeof(digit_t));
        free(v);
        v = vv->u.integer.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->u.integer.data = v;
    vv->u.integer.len = i;
}

static void isub(value_t vv1, value_t vv2, value_t vv) {
    size_t i, len1, len2;
    digit_t *v1, *v2, *v;
    digit_t c;
    int neg;
    len1 = intlen(vv1);
    len2 = intlen(vv2);
    if (len1 <= 1 && len2 <= 1) {
        digit_t d1 = vv1->u.integer.val[0], d2 = vv2->u.integer.val[0];
        v = vv->u.integer.val;
        vv->u.integer.data = v;
        if (d1 < d2) {
            v[0] = d2 - d1;
            vv->u.integer.len = -1;
            return;
        }
        v[0] = d1 - d2;
        vv->u.integer.len = (v[0] != 0);
        return;
    }
    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        neg = 1;
        i = len1; len1 = len2; len2 = i;
    } else {
        neg = 0;
        if (len1 == len2) {
            i = len1;
            v1 = vv1->u.integer.data; v2 = vv2->u.integer.data;
            while (i && v1[i - 1] == v2[i - 1]) i--;
            if (!i) {
                if (vv == vv1 || vv == vv2) destroy(vv);
                vv->u.integer.len = 0;
                vv->u.integer.val[0] = 0;
                vv->u.integer.data = vv->u.integer.val;
                return;
            }
            if (v1[i - 1] < v2[i - 1]) {
                const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
                neg = 1;
            }
            len1 = len2 = i;
        }
    }
    v = inew(vv, len1);
    v1 = vv1->u.integer.data; v2 = vv2->u.integer.data;
    for (c = i = 0; i < len2; i++) {
        if (c) {
            c = v1[i] <= v2[i];
            v[i] = v1[i] - v2[i] - 1;
            continue;
        } 
        c = v1[i] < v2[i];
        v[i] = v1[i] - v2[i];
    }
    for (;c && i < len1; i++) {
        c = v1[i] == 0;
        v[i] = v1[i] - 1;
    }
    for (;i < len1; i++) v[i] = v1[i];
    while (i && !v[i - 1]) i--;
    if (v != vv->u.integer.val && i <= sizeof(vv->u.integer.val)/sizeof(vv->u.integer.val[0])) {
        memcpy(vv->u.integer.val, v, i * sizeof(digit_t));
        free(v);
        v = vv->u.integer.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->u.integer.data = v;
    vv->u.integer.len = neg ? -i : i;
}

static void imul(const value_t vv1, const value_t vv2, value_t vv) {
    size_t i, j, len1, len2, sz;
    digit_t *v1, *v2, *v;
    struct value_s tmp;
    len1 = intlen(vv1);
    len2 = intlen(vv2);
    sz = len1 + len2;
    if (sz < len2) err_msg_out_of_memory(); /* overflow */
    if (sz <= 2) {
        twodigits_t c = (twodigits_t)vv1->u.integer.val[0] * vv2->u.integer.val[0];
        v = vv->u.integer.val;
        vv->u.integer.data = v;
        if (c > MASK) {
            v[0] = (digit_t)c;
            v[1] = c >> SHIFT;
            vv->u.integer.len = 2;
            return;
        }
        v[0] = c;
        vv->u.integer.len = (v[0] != 0);
        return;
    }
    v = inew(&tmp, sz);
    memset(v, 0, sz * sizeof(digit_t));
    v1 = vv1->u.integer.data; v2 = vv2->u.integer.data;
    for (i = 0; i < len1; i++) {
        twodigits_t c = 0, t = v1[i];
        digit_t *o = v + i;
        for (j = 0; j < len2; j++) {
            c += o[j] + v2[j] * t;
            o[j] = (digit_t)c;
            c >>= SHIFT;
        }
        if (c) o[j] += (digit_t)c;
    }
    i = sz;
    while (i && !v[i - 1]) i--;
    if (vv == vv1 || vv == vv2) destroy(vv);
    if (i <= sizeof(vv->u.integer.val)/sizeof(vv->u.integer.val[0])) {
        memcpy(vv->u.integer.val, v, i * sizeof(digit_t));
        if (tmp.u.integer.val != v) free(v);
        v = vv->u.integer.val;
    }
    vv->u.integer.data = v;
    vv->u.integer.len = i;
}

static MUST_CHECK value_t idivrem(const value_t vv1, const value_t vv2, int divrem, linepos_t epoint) {
    size_t len1, len2;
    int neg, negr;
    digit_t *v1, *v2, *v;
    value_t vv;

    len2 = intlen(vv2);
    if (!len2) { 
        return new_error_obj(ERROR_DIVISION_BY_Z, epoint);
    }
    len1 = intlen(vv1);
    v1 = vv1->u.integer.data;
    v2 = vv2->u.integer.data;
    if (len1 < len2 || (len1 == len2 && v1[len1 - 1] < v2[len2 - 1])) {
        return divrem ? val_reference(int_value[0]) : val_reference(vv1);
    }
    negr = (vv1->u.integer.len < 0);
    neg = (negr != (vv2->u.integer.len < 0));
    if (len2 == 1) {
        size_t i;
        twodigits_t r = 0;
        digit_t n = v2[0];
        if (divrem) {
            vv = val_alloc(INT_OBJ);
            v = inew(vv, len1);
            for (i = len1; i--;) {
                digit_t h;
                r = (r << SHIFT) | v1[i];
                h = (digit_t)(r / n);
                v[i] = h;
                r -= (twodigits_t)h * n;
            }
            return normalize(vv, v, len1, neg);
        }
        for (i = len1; i--;) {
            digit_t h;
            r = (r << SHIFT) | v1[i];
            h = (digit_t)(r / n);
            r -= (twodigits_t)h * n;
        }
        if (!r) return val_reference(int_value[0]);
        vv = val_alloc(INT_OBJ);
        vv->u.integer.val[0] = r;
        vv->u.integer.data = vv->u.integer.val;
        vv->u.integer.len = negr ? -1 : 1;
        return vv;
    } else {
        size_t i;
        int k, d;
        digit_t wm1, wm2, c, *v0, *vk, *w0, *ak, *a;
        struct value_s tmp1, tmp2, tmp3;

        if (len1 + 1 < 1) err_msg_out_of_memory(); /* overflow */
        v0 = inew(&tmp1, len1 + 1);
        w0 = inew(&tmp2, len2);

        d = 0;
        while ((v2[len2 - 1] << d) <= MASK / 2) d++;

        if (d) {
            w0[0] = v2[0] << d;
            for (i = 1; i < len2; i++) w0[i] = (v2[i] << d) | (v2[i - 1] >> (SHIFT - d));
            v0[0] = v1[0] << d;
            for (i = 1; i < len1; i++) v0[i] = (v1[i] << d) | (v1[i - 1] >> (SHIFT - d));
            v0[i] = v1[i - 1] >> (SHIFT - d);
        } else {
            memcpy(w0, v2, len2 * sizeof(digit_t));
            v0[len1] = 0;
            memcpy(v0, v1, len1 * sizeof(digit_t));
        }
        if (v0[len1] || v0[len1 - 1] >= w0[len2 - 1]) len1++;
   
        k = len1 - len2;
        a = inew(&tmp3, k);

        wm1 = w0[len2 - 1]; wm2 = w0[len2 - 2];
        for (vk = v0 + k, ak = a + k; vk-- > v0;) {
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
            for (e = c = i = 0; i < len2; i++) {
                digit_t t;
                e += (twodigits_t)q * w0[i];
                t = e; e >>= SHIFT;
                if (c) {
                    c = vk[i] <= t;
                    vk[i] = vk[i] - t - 1;
                    continue;
                } 
                c = vk[i] < t;
                vk[i] -= t;
            }
            if (c ? (vtop <= e) : (vtop < e)) {
                for (c = i = 0; i < len2; i++) {
                    if (c) {
                        c = vk[i];
                        c = (vk[i] = c + w0[i] + 1) <= c;
                        continue;
                    }
                    c = vk[i];
                    c = (vk[i] = c + w0[i]) < c;
                }
                --q;
            }
            *--ak = q;
        }
        if (w0 != tmp2.u.integer.val) free(w0);

        vv = val_alloc(INT_OBJ);
        if (divrem) {
            if (v0 != tmp1.u.integer.val) free(v0);
            while (k && !a[k - 1]) k--;
            if (k <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0])) {
                memcpy(vv->u.integer.val, a, k * sizeof(digit_t));
                if (a != tmp3.u.integer.val) free(a);
                a = vv->u.integer.val;
            }
            vv->u.integer.data = a;
            vv->u.integer.len = neg ? -k : k;
            return vv;
        }

        if (d) {
            for (i = 0; i < len2 - 1; i++) v0[i] = (v0[i] >> d) | (v0[i + 1] << (SHIFT - d));
            v0[i] >>= d;
        } 

        if (a != tmp3.u.integer.val) free(a);
        while (len2 && !v0[len2 - 1]) len2--;
        if (len2 <= sizeof(vv->u.integer.val)/sizeof(vv->u.integer.val[0])) {
            memcpy(vv->u.integer.val, v0, len2 * sizeof(digit_t));
            if (v0 != tmp1.u.integer.val) free(v0);
            v0 = vv->u.integer.val;
        }
        vv->u.integer.data = v0;
        vv->u.integer.len = negr ? -len2 : len2;
        return vv;
    }
}

static MUST_CHECK value_t power(const value_t vv1, const value_t vv2) {
    int j, neg = 0;
    size_t i;
    value_t v = int_from_int(1);

    for (i = vv2->u.integer.len; i--;) {
        digit_t d = vv2->u.integer.data[i];
        for (j = SHIFT - 1; j >= 0; j--) {
            imul(v, v, v);
            if (d & (1 << j)) {
                imul(v, vv1, v);
                neg = 1;
            } else neg = 0;
        }
    }
    if (neg && vv1->u.integer.len < 0) v->u.integer.len = -v->u.integer.len;
    return v;
}

static MUST_CHECK value_t ilshift(const value_t vv1, uval_t s) {
    size_t i, len1, sz;
    int word, bit;
    digit_t *v1, *v, *v2;
    value_t vv;

    word = s / SHIFT;
    bit = s % SHIFT;
    v1 = vv1->u.integer.data;
    len1 = intlen(vv1);
    sz = len1 + word + (bit > 0);
    vv = val_alloc(INT_OBJ);
    v = inew(vv, sz);
    v2 = v + word;
    if (bit) {
        v2[len1] = 0;
        for (i = len1; i--;) {
            v2[i + 1] |= v1[i] >> (SHIFT - bit);
            v2[i] = (digit_t)(v1[i] << bit);
        }
    } else if (len1) memcpy(v2, v1, len1 * sizeof(digit_t));
    if (word) memset(v, 0, word * sizeof(digit_t));

    return normalize(vv, v, sz, vv1->u.integer.len < 0);
}

static MUST_CHECK value_t irshift(value_t vv1, uval_t s) {
    size_t i, sz;
    int word, bit, neg;
    digit_t *v1, *v;
    value_t vv;

    word = s / SHIFT;
    bit = s % SHIFT;
    neg = (vv1->u.integer.len < 0);
    if (neg) {
        vv = val_alloc(INT_OBJ);
        isub(vv1, int_value[1], vv);
        vv1 = vv;
        if (vv->u.integer.len <= word) {
            val_destroy(vv);
            return int_from_int(-1);
        }
        sz = vv->u.integer.len - word;
    } else {
        if (vv1->u.integer.len <= word) return val_reference(int_value[0]);
        sz = vv1->u.integer.len - word;
        vv = val_alloc(INT_OBJ);
    }
    v = inew(vv, sz);
    v1 = vv1->u.integer.data + word;
    if (bit) {
        for (i = 0; i < sz - 1; i++) {
            v[i] = v1[i] >> bit;
            v[i] |= (digit_t)(v1[i + 1] << (SHIFT - bit));
        }
        v[i] = v1[i] >> bit;
    } else if (sz) memcpy(v, v1, sz * sizeof(digit_t));

    if (neg) {
        vv->u.integer.data = v;
        vv->u.integer.len = sz;
        iadd(int_value[1], vv, vv);
        vv->u.integer.len = -vv->u.integer.len;
        return vv;
    }
    return normalize(vv, v, sz, 0);
}

static MUST_CHECK value_t iand(value_t vv1, value_t vv2) {
    size_t i, len1, len2, sz;
    int neg1, neg2;
    digit_t *v1, *v2, *v;
    digit_t c;
    value_t vv = val_alloc(INT_OBJ);
    len1 = intlen(vv1);
    len2 = intlen(vv2);

    if (len1 <= 1 && len2 <= 1) {
        neg1 = (vv1->u.integer.len < 0); neg2 = (vv2->u.integer.len < 0);
        c = neg1 ? -vv1->u.integer.val[0] : vv1->u.integer.val[0];
        c &= neg2 ? -vv2->u.integer.val[0] : vv2->u.integer.val[0];
        neg1 &= neg2;
        if (neg1) c = -c;
        v = vv->u.integer.val;
        vv->u.integer.data = v;
        v[0] = c;
        vv->u.integer.len = neg1 ? -(v[0] != 0) : (v[0] != 0);
        return vv;
    }
    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->u.integer.data; v2 = vv2->u.integer.data;
    neg1 = (vv1->u.integer.len < 0); neg2 = (vv2->u.integer.len < 0);

    sz = (neg2 ? len1 : len2) + (neg1 & neg2);
    v = inew(vv, sz);

    if (neg1) {
        if (neg2) {
            int c1 = 1, c2 = 1;
            for (c = 1, i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c1) {
                    c1 = e == 0;
                    if (c2) {
                        c2 = f == 0;
                        g = (~e + 1) & (~f + 1);
                    } else g = (~e + 1) & ~f;
                } else {
                    if (c2) {
                        c2 = f == 0;
                        g = ~e & (~f + 1);
                    } else g = ~e & ~f;
                }
                if (c) {
                    c = g == 0;
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            if (c2) {
                if (c) for (; i < len1; i++) v[i] = 0;
                else for (; i < len1; i++) v[i] = ~0;
            } else {
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
            v[i] = c;
        } else {
            int c1 = 1;
            for (i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i];
                if (c1) {
                    c1 = e == 0;
                    v[i] = (~e + 1) & f;
                    continue;
                }
                v[i] = ~e & f;
            }
        }
    } else {
        if (neg2) {
            int c2 = 1;
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
    return normalize(vv, v, sz, neg1 & neg2);
}

static MUST_CHECK value_t ior(value_t vv1, value_t vv2) {
    size_t i, len1, len2, sz;
    int neg1, neg2;
    digit_t *v1, *v2, *v;
    digit_t c;
    value_t vv = val_alloc(INT_OBJ);
    len1 = intlen(vv1);
    len2 = intlen(vv2);

    if (len1 <= 1 && len2 <= 1) {
        neg1 = (vv1->u.integer.len < 0); neg2 = (vv2->u.integer.len < 0);
        c = neg1 ? -vv1->u.integer.val[0] : vv1->u.integer.val[0];
        c |= neg2 ? -vv2->u.integer.val[0] : vv2->u.integer.val[0];
        neg1 |= neg2;
        if (neg1) c = -c;
        v = vv->u.integer.val;
        vv->u.integer.data = v;
        v[0] = c;
        vv->u.integer.len = neg1 ? -(v[0] != 0) : (v[0] != 0);
        return vv;
    }
    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->u.integer.data; v2 = vv2->u.integer.data;
    neg1 = (vv1->u.integer.len < 0); neg2 = (vv2->u.integer.len < 0);

    sz = (neg2 ? len2 : len1) + (neg1 | neg2);
    v = inew(vv, sz);

    if (neg1) {
        if (neg2) {
            int c1 = 1, c2 = 1;
            for (c = 1, i = 0; i < len2; i++) {
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
            int c1 = 1;
            for (c = 1, i = 0; i < len2; i++) {
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
        v[i] = c;
    } else {
        if (neg2) {
            int c2 = 1;
            for (c = 1, i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c2) {
                    c2 = f == 0;
                    g = e | (~f + 1);
                } else g = e | ~f;
                if (c) {
                    c = g == 0;
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            v[i] = c;
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] | v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    }
    return normalize(vv, v, sz, neg1 | neg2);
}

static MUST_CHECK value_t ixor(value_t vv1, value_t vv2) {
    size_t i, len1, len2, sz;
    int neg1, neg2;
    digit_t *v1, *v2, *v;
    digit_t c;
    value_t vv = val_alloc(INT_OBJ);
    len1 = intlen(vv1);
    len2 = intlen(vv2);

    if (len1 <= 1 && len2 <= 1) {
        neg1 = (vv1->u.integer.len < 0); neg2 = (vv2->u.integer.len < 0);
        c = neg1 ? -vv1->u.integer.val[0] : vv1->u.integer.val[0];
        c ^= neg2 ? -vv2->u.integer.val[0] : vv2->u.integer.val[0];
        neg1 ^= neg2;
        if (neg1) c = -c;
        v = vv->u.integer.val;
        vv->u.integer.data = v;
        v[0] = c;
        vv->u.integer.len = neg1 ? -(v[0] != 0) : (v[0] != 0);
        return vv;
    }
    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->u.integer.data; v2 = vv2->u.integer.data;
    neg1 = (vv1->u.integer.len < 0); neg2 = (vv2->u.integer.len < 0);

    sz = len1 + (neg1 ^ neg2);
    v = inew(vv, sz);

    if (neg1) {
        if (neg2) {
            int c1 = 1, c2 = 1;
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
            int c1 = 1;
            for (c = 1, i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c1) {
                    c1 = e == 0;
                    g = (~e + 1) ^ f;
                } else g = ~e ^ f;
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
            v[i] = c;
        }
    } else {
        if (neg2) {
            int c2 = 1;
            for (c = 1, i = 0; i < len2; i++) {
                digit_t e = v1[i], f = v2[i], g;
                if (c2) {
                    c2 = f == 0;
                    g = e ^ (~f + 1);
                } else g = e ^ ~f;
                if (c) {
                    c = g == 0;
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            for (; i < len1; i++) {
                digit_t e = v1[i], g;
                g = c2 ? e : ~e;
                if (c) {
                    c = g == 0;
                    v[i] = ~g + 1;
                    continue;
                }
                v[i] = ~g;
            }
            v[i] = c;
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    }
    return normalize(vv, v, sz, neg1 ^ neg2);
}

static ssize_t icmp(const value_t vv1, const value_t vv2) {
    ssize_t i;
    size_t j;
    digit_t a, b;
    i = vv1->u.integer.len - vv2->u.integer.len;
    if (i) return i;
    j = intlen(vv1);
    while (j--) {
        a = vv1->u.integer.data[j]; b = vv2->u.integer.data[j];
        if (a > b) return (vv1->u.integer.len < 0) ? -1 : 1;
        if (a < b) return (vv1->u.integer.len < 0) ? 1 : -1;
    }
    return 0;
}

MUST_CHECK value_t int_from_int(int i) {
    value_t v = val_alloc(INT_OBJ);
    v->u.integer.data = v->u.integer.val;
    if (i < 0) {
        v->u.integer.val[0] = -i;
        v->u.integer.len = -1;
        return v;
    }
    v->u.integer.val[0] = i;
    v->u.integer.len = (i != 0);
    return v;
}

MUST_CHECK value_t int_from_size(size_t i) {
    unsigned int j;
    value_t v;
    if (i < sizeof(int_value) / sizeof(int_value[0])) return val_reference(int_value[i]);
    v = val_alloc(INT_OBJ);
    v->u.integer.data = v->u.integer.val;
    v->u.integer.val[0] = i;
    for (j = 1; j < sizeof(size_t) / sizeof(digit_t); j++) {
        i >>= 4 * sizeof(digit_t);
        i >>= 4 * sizeof(digit_t);
        if (!i) break;
        v->u.integer.val[j] = i;
    }
    v->u.integer.len = j;
    return v;
}

MUST_CHECK value_t int_from_uval(uval_t i) {
    value_t v = val_alloc(INT_OBJ);
    v->u.integer.data = v->u.integer.val;
    v->u.integer.val[0] = i;
    v->u.integer.len = (i != 0);
    return v;
}

MUST_CHECK value_t int_from_ival(ival_t i) {
    value_t v = val_alloc(INT_OBJ);
    v->u.integer.data = v->u.integer.val;
    if (i < 0) {
        v->u.integer.val[0] = -i;
        v->u.integer.len = -1;
        return v;
    }
    v->u.integer.val[0] = i;
    v->u.integer.len = (i != 0);
    return v;
}

MUST_CHECK value_t int_from_float(const value_t v1) {
    int neg, expo;
    double frac, f = v1->u.real;
    size_t sz;
    digit_t *d;
    value_t v;

    neg = (f < 0.0);
    if (neg) f = -f;

    frac = frexp(f, &expo);
    if (expo <= 0) {
        return val_reference(int_value[0]);
    }
    sz = (expo - 1) / SHIFT + 1;

    v = val_alloc(INT_OBJ);
    d = inew(v, sz);
    v->u.integer.len = neg ? -sz : +sz;
    v->u.integer.data = d;

    frac = ldexp(frac, (expo - 1) % SHIFT + 1);

    while (sz--) {
        digit_t dg = (digit_t)frac;
        d[sz] = dg;
        frac = ldexp(frac - (double)dg, SHIFT);
    }
    return v;
}

MUST_CHECK value_t int_from_bytes(const value_t v1) {
    int bits;
    size_t i, j, sz, len1;
    digit_t *d, uv;
    value_t v;
    int inv;

    switch (v1->u.bytes.len) {
    case 0: return val_reference(int_value[0]);
    case ~0: return int_from_int(-1);
    }

    inv = v1->u.bytes.len < 0;
    len1 = inv ? -v1->u.bytes.len : v1->u.bytes.len; /* it's - for the additional length  */
    sz = len1 / sizeof(digit_t);
    if (len1 % sizeof(digit_t)) sz++;

    v = val_alloc(INT_OBJ);
    d = inew(v, sz);

    uv = bits = j = i = 0;
    if (inv) {
        uint8_t c = 0xff;
        for (;c == 0xff && i < len1 - 1; i++) {
            c = v1->u.bytes.data[i];
            uv |= ((uint8_t)(c + 1)) << bits;
            if (bits == SHIFT - 8) {
                d[j++] = uv;
                bits = uv = 0;
            } else bits += 8;
        }
        for (; i < len1 - 1; i++) {
            uv |= v1->u.bytes.data[i] << bits;
            if (bits == SHIFT - 8) {
                d[j++] = uv;
                bits = uv = 0;
            } else bits += 8;
        }
        uv |= (c == 0xff) << bits;
        d[j] = uv;
    } else {
        for (;i < len1; i++) {
            uv |= v1->u.bytes.data[i] << bits;
            if (bits == SHIFT - 8) {
                d[j++] = uv;
                bits = uv = 0;
            } else bits += 8;
        }
        if (bits) d[j] = uv;
    }

    return normalize(v, d, sz, inv);
}

MUST_CHECK value_t int_from_bits(const value_t v1) {
    unsigned int inv;
    size_t i, sz;
    digit_t *d;
    const bdigit_t *b;
    value_t v;

    switch (v1->u.bits.len) {
    case 0: return val_reference(int_value[0]);
    case ~0: return int_from_int(-1);
    }

    inv = v1->u.bits.len < 0;
    sz = inv ? -v1->u.bits.len : v1->u.bits.len; /* it's - for the additional length  */
    if (sz < inv) err_msg_out_of_memory(); /* overflow */
    v = val_alloc(INT_OBJ);
    d = inew(v, sz);

    b = v1->u.bits.data;
    if (inv) {
        int c = 1;
        for (i = 0; c && i < sz - 1; i++) {
            c = (d[i] = b[i] + 1) < 1;
        }
        for (; i < sz - 1; i++) {
            d[i] = b[i];
        }
        d[i] = c;
    } else memcpy(d, b, sz * sizeof(digit_t));

    return normalize(v, d, sz, inv);
}

MUST_CHECK value_t int_from_str(const value_t v1, linepos_t epoint) {
    int ch;
    value_t v;

    if (actual_encoding) {
        digit_t uv;
        int bits;
        size_t i, j, sz, osz;
        digit_t *d;

        i = v1->u.str.len;
        if (!i) {
            return val_reference(int_value[0]);
        }

        sz = i / sizeof(digit_t);
        if (i % sizeof(digit_t)) sz++;
        v = val_alloc(INT_OBJ);
        d = inew(v, sz);

        uv = bits = j = 0;
        encode_string_init(v1, epoint);
        while ((ch = encode_string()) != EOF) {
            uv |= (uint8_t)ch << bits;
            if (bits == SHIFT - 8) {
                if (j >= sz) {
                    if (v->u.integer.val == d) {
                        sz = 16 / sizeof(digit_t);
                        d = (digit_t *)malloc(sz * sizeof(digit_t));
                        memcpy(d, v->u.integer.val, j * sizeof(digit_t));
                    } else {
                        sz += 1024 / sizeof(digit_t);
                        if (sz < 1024 / sizeof(digit_t)) err_msg_out_of_memory(); /* overflow */
                        d = (digit_t *)realloc(d, sz * sizeof(digit_t));
                    }
                    if (!d) err_msg_out_of_memory();
                }
                d[j++] = uv;
                bits = uv = 0;
            } else bits += 8;
        }
        if (bits) {
            if (j >= sz) {
                sz++;
                if (sz < 1) err_msg_out_of_memory(); /* overflow */
                if (v->u.integer.val == d) {
                    d = (digit_t *)malloc(sz * sizeof(digit_t));
                    memcpy(d, v->u.integer.val, j * sizeof(digit_t));
                } else d = (digit_t *)realloc(d, sz * sizeof(digit_t));
                if (!d) err_msg_out_of_memory();
            }
            d[j] = uv;
            osz = j + 1;
        } else osz = j;

        while (osz && !d[osz - 1]) osz--;
        if (v->u.integer.val != d) {
            if (osz <= sizeof(v->u.integer.val)/sizeof(v->u.integer.val[0])) {
                memcpy(v->u.integer.val, d, osz * sizeof(digit_t));
                free(d);
                d = v->u.integer.val;
            } else if (osz < sz) {
                d = (digit_t *)realloc(d, osz * sizeof(digit_t));
                if (!d) err_msg_out_of_memory();
            }
        }
        v->u.integer.data = d;
        v->u.integer.len = osz;
        return v;
    } 
    if (v1->u.str.chars == 1) {
        uint32_t ch2 = v1->u.str.data[0];
        if (ch2 & 0x80) utf8in(v1->u.str.data, &ch2);
        return int_from_uval(ch2);
    } 
    return new_error_obj(ERROR_BIG_STRING_CO, epoint);
}

MUST_CHECK value_t int_from_decstr(const uint8_t *s, size_t *ln) {
    const uint8_t *end;
    size_t i = 0, j, sz, l;
    digit_t *d, *end2;
    value_t v;

    while ((s[i] ^ 0x30) < 10) i++;
    if (i < 10) {
        digit_t val;
        if (!i) val = 0; else {
            val = s[0] & 15; 
            for (j = 1;j < i; j++) val = val * 10 + (s[j] & 15);
        }
        *ln = i;
        if (val >= sizeof(int_value) / sizeof(int_value[0])) {
            v = val_alloc(INT_OBJ);
            v->u.integer.val[0] = val;
            v->u.integer.data = v->u.integer.val;
            v->u.integer.len = 1;
            return v;
        }
        return val_reference(int_value[val]);
    }
    sz = (double)i * 0.11073093649624542178511177326072356663644313812255859375 + 1;
    l = i;

    v = val_alloc(INT_OBJ);
    d = inew(v, sz);

    end = s + i;
    end2 = d;
    while (s < end) {
        digit_t *d2 = d;
        twodigits_t mul, a = *s++ & 15;
        for (j = 1; j < 9 && s < end; j++) a = a * 10 + (*s++ & 15);
        if (j == 9) mul = 1000000000;
        else {
            mul = 10;
            while (--j) mul *= 10;
        }
        while (d2 < end2) {
            twodigits_t m = *d2 * mul;
            a += m;
            *d2++ = (digit_t)a;
            a >>= SHIFT;
        }
        if (a) {
            if (end2 >= &d[sz]) {
                sz++;
                if (sz > sizeof(v->u.integer.val)/sizeof(v->u.integer.val[0])) {
                    if (d == v->u.integer.val) { 
                        d = (digit_t *)malloc(sz * sizeof(digit_t));
                        if (!d) err_msg_out_of_memory();
                        memcpy(d, v->u.integer.val, sizeof(v->u.integer.val));
                    } else {
                        d = (digit_t *)realloc(d, sz * sizeof(digit_t));
                        if (!d || sz > SSIZE_MAX / sizeof(digit_t)) err_msg_out_of_memory(); /* overflow */
                    }
                }
                end2 = d + sz - 1;
            }
            *end2++ = a;
        }
    }

    sz = end2 - d;
    *ln = l;
    return normalize(v, d, sz, 0);
}

static MUST_CHECK value_t calc2_int(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    value_t err;
    ival_t shift;
    int i;
    ssize_t val;
    switch (op->op->u.oper.op) {
    case O_CMP:
        val = icmp(v1, v2);
        if (val < 0) return int_from_int(-1);
        return val_reference(int_value[val > 0]);
    case O_EQ: return truth_reference(icmp(v1, v2) == 0);
    case O_NE: return truth_reference(icmp(v1, v2) != 0);
    case O_LT: return truth_reference(icmp(v1, v2) < 0);
    case O_LE: return truth_reference(icmp(v1, v2) <= 0);
    case O_GT: return truth_reference(icmp(v1, v2) > 0);
    case O_GE: return truth_reference(icmp(v1, v2) >= 0);
    case O_ADD:
        v = val_alloc(INT_OBJ);
        if (v1->u.integer.len < 0) {
            if (v2->u.integer.len < 0) {
                iadd(v1, v2, v);
                v->u.integer.len = -v->u.integer.len;
            } else isub(v2, v1, v);
        } else {
            if (v2->u.integer.len < 0) isub(v1, v2, v);
            else iadd(v1, v2, v);
        }
        return v;
    case O_SUB:
        v = val_alloc(INT_OBJ);
        if (v1->u.integer.len < 0) {
            if (v2->u.integer.len < 0) isub(v1, v2, v);
            else iadd(v1, v2, v);
            v->u.integer.len = -v->u.integer.len;
        } else {
            if (v2->u.integer.len < 0) iadd(v1, v2, v);
            else isub(v1, v2, v);
        }
        return v;
    case O_MUL:
        v = val_alloc(INT_OBJ);
        if ((v1->u.integer.len ^ v2->u.integer.len) < 0) {
            imul(v1, v2, v);
            v->u.integer.len = -v->u.integer.len;
        } else imul(v1, v2, v);
        return v;
    case O_DIV:
        i = (v2->u.integer.len < 0);
        v = idivrem(v1, v2, 1, op->epoint3);
        if (v->obj != INT_OBJ) return v;
        if ((v1->u.integer.len < 0) ^ i) {
            value_t vv = val_alloc(INT_OBJ);
            if (v->u.integer.len < 0) {
                iadd(v, int_value[1], vv);
                vv->u.integer.len = -vv->u.integer.len;
            } else isub(v, int_value[1], vv);
            val_destroy(v); 
            return vv;
        }
        return v;
    case O_MOD:
        v = idivrem(v1, v2, 0, op->epoint3);
        if (v->obj != INT_OBJ) return v;
        if ((v->u.integer.len < 0) ^ (v2->u.integer.len < 0)) {
            value_t vv = val_alloc(INT_OBJ);
            if (v->u.integer.len < 0) {
                if (v2->u.integer.len < 0) {
                    iadd(v, v2, vv);
                    vv->u.integer.len = -vv->u.integer.len;
                } else isub(v2, v, vv);
            } else {
                if (v2->u.integer.len < 0) isub(v, v2, vv);
                else iadd(v, v2, vv);
            }
            val_destroy(v);
            return vv;
        }
        return v;
    case O_EXP:
        if (v2->u.integer.len < 0) {
            double d1, d2;
            err = float_from_int(v1, op->epoint);
            if (err->obj != FLOAT_OBJ) return err;
            d1 = err->u.real;
            val_destroy(err);
            err = float_from_int(v2, op->epoint);
            if (err->obj != FLOAT_OBJ) return err;
            d2 = err->u.real;
            val_destroy(err);
            return calc2_double(op, d1, d2);
        }
        return power(v1, v2);
    case O_LSHIFT:
        err = ival(v2, &shift, 8*sizeof(ival_t), op->epoint2);
        if (err) return err;
        return (shift < 0) ? irshift(v1, -shift) : ilshift(v1, shift);
    case O_RSHIFT:
        err = ival(v2, &shift, 8*sizeof(ival_t), op->epoint2);
        if (err) return err;
        return (shift < 0) ? ilshift(v1, -shift) : irshift(v1, shift);
    case O_AND: return iand(v1, v2);
    case O_OR: return ior(v1, v2);
    case O_XOR: return ixor(v1, v2);
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t calc2(oper_t op) {
    value_t tmp, ret, v2 = op->v2;
    switch (v2->obj->type) {
    case T_INT: return calc2_int(op);
    case T_BOOL:
        tmp = val_reference(int_value[v2->u.boolean]);
        op->v2 = tmp;
        ret = calc2_int(op);
        val_destroy(tmp);
        op->v2 = v2;
        return ret;
    case T_BYTES:
        tmp = int_from_bytes(v2);
        op->v2 = tmp;
        ret = calc2(op);
        val_destroy(tmp);
        op->v2 = v2;
        return ret;
    case T_BITS:
        tmp = int_from_bits(v2);
        op->v2 = tmp;
        ret = calc2(op);
        val_destroy(tmp);
        op->v2 = v2;
        return ret;
    case T_STR:
        tmp = int_from_str(v2, op->epoint2);
        op->v2 = tmp;
        ret = calc2(op);
        val_destroy(tmp);
        op->v2 = v2;
        return ret;
    default: 
        if (op->op != &o_MEMBER && op->op != &o_INDEX && op->op != &o_X) {
            return v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    value_t tmp, ret, v1 = op->v1;
    switch (v1->obj->type) {
    case T_BOOL:
        switch (op->op->u.oper.op) {
        case O_LSHIFT:
        case O_RSHIFT: tmp = bits_from_bool(v1->u.boolean); break;
        default: tmp = val_reference(int_value[v1->u.boolean]); break;
        }
        op->v1 = tmp;
        ret = tmp->obj->calc2(op);
        val_destroy(tmp);
        op->v1 = v1;
        return ret;
    default: break;
    }
    return obj_oper_error(op);
}

void intobj_init(void) {
    obj_init(&obj, T_INT, "int");
    obj.create = create;
    obj.destroy = destroy;
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
