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
#include "eval.h"
#include "unicode.h"
#include "encoding.h"

#include "strobj.h"
#include "boolobj.h"
#include "floatobj.h"
#include "addressobj.h"

#define SHIFT 30
#define MASK ((1 << SHIFT)-1)
#define DSHIFT 9
#define DMUL ((digit_t)1000000000)

static struct obj_s obj;

obj_t INT_OBJ = &obj;

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

static void copy(const value_t v1, value_t v) {
    v->obj = INT_OBJ;
    v->u.integer.len = v1->u.integer.len;
    if (v1->u.integer.len) {
        v->u.integer.data = inew(v, abs(v->u.integer.len));
        memcpy(v->u.integer.data, v1->u.integer.data, abs(v->u.integer.len) * sizeof(digit_t));
    } else {
        v->u.integer.data = v->u.integer.val;
        v->u.integer.val[0] = 0;
    }
}

static int same(const value_t v1, const value_t v2) {
    if (v2->obj != INT_OBJ || v1->u.integer.len != v2->u.integer.len) return 0;
    return !memcmp(v1->u.integer.data, v2->u.integer.data, abs(v1->u.integer.len) * sizeof(digit_t));
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
            h = (h << SHIFT) | (h >> (8 * sizeof(unsigned int) - SHIFT));
            h += v1->u.integer.val[l];
        }
    } else {
        while (l++) {
            h = (h << SHIFT) | (h >> (8 * sizeof(unsigned int) - SHIFT));
            h -= v1->u.integer.val[l];
        }
    }
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t UNUSED(epoint)) {
    ssize_t len = abs(v1->u.integer.len);
    int neg = v1->u.integer.len < 0;
    uint8_t *s;
    digit_t ten, r, *out;
    size_t slen, i, j, sz, len2;
    struct value_s tmp;
    value_t v = val_alloc();

    if (len <= 1) {
        char tmp2[sizeof(digit_t) * 3];
        if (len) len = sprintf(tmp2, neg ? "-%d" : "%d", v1->u.integer.val[0]);
        else {tmp2[0]='0';len = 1;}
        s = str_create_elements(v, len);
        memcpy(s, tmp2, len);
        v->obj = STR_OBJ;
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

    v->obj = STR_OBJ;
    v->u.str.data = s;
    v->u.str.len = slen;
    v->u.str.chars = v->u.str.len;
    return v;
}

static MUST_CHECK value_t ival(const value_t v1, ival_t *iv, int bits, linepos_t epoint) {
    value_t v;
    switch (v1->u.integer.len) {
    case 2: 
        if (v1->u.integer.data[1] >= (~(uval_t)1 << (8*sizeof(uval_t) - SHIFT - 1))) break;
        *iv = v1->u.integer.data[0] | (v1->u.integer.data[1] << SHIFT);
        if (bits < SHIFT) break;
        if (bits < (SHIFT*2-1) && v1->u.integer.data[1] >> (bits - SHIFT - 1)) break;
        return NULL;
    case 1: *iv = v1->u.integer.data[0];
            if (bits < (SHIFT-1) && *iv >> (bits-1)) break;
            return NULL;
    case 0: *iv = 0; return NULL;
    case -1: *iv = -v1->u.integer.data[0];
             if (bits < (SHIFT-1) && ~*iv >> (bits-1)) break;
             return NULL;
    case -2: 
        if (v1->u.integer.data[1] > (~(uval_t)1 << (8*sizeof(uval_t) - SHIFT - 1))) break;
        *iv = -(v1->u.integer.data[0] | (v1->u.integer.data[1] << SHIFT));
        if (bits < SHIFT) break;
        if (bits < (SHIFT*2-1) && ~*iv >> (bits-1)) break;
        return NULL;
    default: break;
    }
    v = val_alloc();
    v->obj = ERROR_OBJ;
    v->u.error.num = ERROR_____CANT_IVAL;
    v->u.error.u.bits = bits;
    v->u.error.epoint = *epoint;
    return v;
}

static MUST_CHECK value_t uval(const value_t v1, uval_t *uv, int bits, linepos_t epoint) {
    value_t v;
    switch (v1->u.integer.len) {
    case 2:
        if (v1->u.integer.data[1] >= (~(uval_t)1 << (8*sizeof(uval_t) - SHIFT))) break;
        *uv = v1->u.integer.data[0] | (v1->u.integer.data[1] << SHIFT);
        if (bits < SHIFT) break;
        if (bits < SHIFT*2 && v1->u.integer.data[1] >> (bits - SHIFT)) break;
        return NULL;
    case 1: *uv = v1->u.integer.data[0];
            if (bits < SHIFT && *uv >> bits) break;
            return NULL;
    case 0: *uv = 0; return NULL;
    default: break;
    }
    v = val_alloc();
    v->obj = ERROR_OBJ;
    v->u.error.num = ERROR_____CANT_UVAL;
    v->u.error.u.bits = bits;
    v->u.error.epoint = *epoint;
    return v;
}

static MUST_CHECK value_t real(const value_t v1, double *r, linepos_t epoint) {
    ssize_t i, len1 = abs(v1->u.integer.len);
    double d = 0;
    for (i = 0; i < len1; i++) {
        if (v1->u.integer.len < 0) d -= ldexp((double)v1->u.integer.data[i], i * SHIFT);
        else d += ldexp((double)v1->u.integer.data[i], i * SHIFT);
        if (d == HUGE_VAL) {
            value_t v = val_alloc();
            v->obj = ERROR_OBJ;
            v->u.error.num = ERROR_____CANT_REAL;
            v->u.error.epoint = *epoint;
            v->u.error.u.objname = v1->obj->name;
            return v;
        }
    }
    *r = d;
    return NULL;
}

static MUST_CHECK value_t sign(const value_t v1, linepos_t UNUSED(epoint)) {
    if (v1->u.integer.len < 0) return int_from_int(-1);
    return val_reference(int_value[v1->u.integer.len > 0]);
}

static MUST_CHECK value_t absolute(const value_t v1, linepos_t UNUSED(epoint)) {
    value_t v = val_alloc();
    copy(v1, v);
    if (v->u.integer.len < 0) v->u.integer.len = -v->u.integer.len;
    return v;
}

static MUST_CHECK value_t integer(const value_t v1, linepos_t UNUSED(epoint)) {
    value_t v = val_alloc();
    copy(v1, v);
    return v;
}

static void iadd(const value_t, const value_t, value_t);
static void isub(const value_t, const value_t, value_t);

static MUST_CHECK value_t calc1(oper_t op) {
    value_t v1 = op->v1, v;
    uval_t uv;
    switch (op->op->u.oper.op) {
    case O_BANK: 
        uv = v1->u.integer.len ? v1->u.integer.data[0] : 0;
        if (v1->u.integer.len < 0) uv = -uv;
        return bits_from_u8(uv >> 16);
    case O_HIGHER:
        uv = v1->u.integer.len ? v1->u.integer.data[0] : 0;
        if (v1->u.integer.len < 0) uv = -uv;
        return bits_from_u8(uv >> 8);
    case O_LOWER:
        uv = v1->u.integer.len ? v1->u.integer.data[0] : 0;
        if (v1->u.integer.len < 0) uv = -uv;
        return bits_from_u8(uv);
    case O_HWORD:
        uv = v1->u.integer.len ? v1->u.integer.data[0] : 0;
        if (v1->u.integer.len < 0) uv = -uv;
        return bits_from_u16(uv >> 8);
    case O_WORD:
        uv = v1->u.integer.len ? v1->u.integer.data[0] : 0;
        if (v1->u.integer.len < 0) uv = -uv;
        return bits_from_u16(uv);
    case O_BSWORD:
        uv = v1->u.integer.len ? v1->u.integer.data[0] : 0;
        if (v1->u.integer.len < 0) uv = -uv;
        return bits_from_u16((uint8_t)(uv >> 8) | (uint16_t)(uv << 8));
    case O_INV:
        v = val_alloc();
        v->obj = INT_OBJ;
        if (v1->u.integer.len < 0) isub(v1, int_value[1], v);
        else {
            iadd(v1, int_value[1], v);
            v->u.integer.len = -v->u.integer.len;
        }
        return v;
    case O_NEG:
        v = val_alloc();
        copy(v1, v);
        v->u.integer.len = -v->u.integer.len;
        return v;
    case O_POS:
        v = val_alloc();
        copy(v1, v);
        return v;
    case O_STRING: return repr(v1, op->epoint);
    default: break;
    }
    return obj_oper_error(op);
}

static void iadd(value_t vv1, value_t vv2, value_t vv) {
    ssize_t i, len1, len2;
    digit_t *v1, *v2, *v;
    digit_t c;
    len1 = abs(vv1->u.integer.len);
    len2 = abs(vv2->u.integer.len);
    if (len1 <= 1 && len2 <= 1) {
        c = vv1->u.integer.val[0] + vv2->u.integer.val[0];
        v = vv->u.integer.val;
        vv->u.integer.data = v;
        if (c > MASK) {
            v[0] = c & MASK;
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
        c += v1[i] + v2[i];
        v[i] = c & MASK;
        c = c > MASK;
    }
    for (;c && i < len1; i++) {
        c += v1[i];
        v[i] = c & MASK;
        c = c > MASK;
    }
    for (;i < len1; i++) v[i] = v1[i];
    if (c) v[i++] = c;
    while (i && !v[i - 1]) i--;
    if (i <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0]) && v != vv->u.integer.val) {
        memcpy(vv->u.integer.val, v, i * sizeof(digit_t));
        free(v);
        v = vv->u.integer.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->u.integer.data = v;
    vv->u.integer.len = i;
}

static void isub(value_t vv1, value_t vv2, value_t vv) {
    ssize_t i, len1, len2;
    digit_t *v1, *v2, *v;
    digit_t c;
    int neg;
    len1 = abs(vv1->u.integer.len);
    len2 = abs(vv2->u.integer.len);
    if (len1 <= 1 && len2 <= 1) {
        c = vv1->u.integer.val[0] - vv2->u.integer.val[0];
        v = vv->u.integer.val;
        vv->u.integer.data = v;
        if (c > MASK) {
            v[0] = -c;
            vv->u.integer.len = -1;
            return;
        }
        v[0] = c;
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
        c = v1[i] - v2[i] - c;
        v[i] = c & MASK;
        c = c > MASK;
    }
    for (;c && i < len1; i++) {
        c = v1[i] - c;
        v[i] = c & MASK;
        c = c > MASK;
    }
    for (;i < len1; i++) v[i] = v1[i];
    while (i && !v[i - 1]) i--;
    if (i <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0]) && v != vv->u.integer.val) {
        memcpy(vv->u.integer.val, v, i * sizeof(digit_t));
        free(v);
        v = vv->u.integer.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->u.integer.data = v;
    vv->u.integer.len = neg ? -i : i;
}

static void imul(const value_t vv1, const value_t vv2, value_t vv) {
    ssize_t i, j, len1, len2, sz;
    digit_t *v1, *v2, *v;
    struct value_s tmp;
    len1 = abs(vv1->u.integer.len);
    len2 = abs(vv2->u.integer.len);
    sz = len1 + len2;
    if (sz < len2) err_msg_out_of_memory(); /* overflow */
    if (sz <= 2) {
        twodigits_t c = (twodigits_t)vv1->u.integer.val[0] * vv2->u.integer.val[0];
        v = vv->u.integer.val;
        vv->u.integer.data = v;
        if (c > MASK) {
            v[0] = c & MASK;
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
            o[j] = (digit_t)(c & MASK);
            c >>= SHIFT;
        }
        if (c) o[j] += (digit_t)(c & MASK);
    }
    i = sz;
    while (i && !v[i - 1]) i--;
    if (vv == vv1 || vv == vv2) destroy(vv);
    if (i <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0])) {
        memcpy(vv->u.integer.val, v, i * sizeof(digit_t));
        if (tmp.u.integer.val != v) free(v);
        v = vv->u.integer.val;
    }
    vv->u.integer.data = v;
    vv->u.integer.len = i;
}

static void idivrem(const value_t vv1, const value_t vv2, value_t vv, value_t rem) {
    ssize_t len1, len2;
    int neg, negr;
    len2 = abs(vv2->u.integer.len);
    digit_t *v1, *v2, *v;
    if (!len2) { 
        if (vv1 == rem) destroy(vv);
        rem->obj = vv->obj = ERROR_OBJ;
        rem->u.error.num = vv->u.error.num = ERROR_DIVISION_BY_Z; return; 
    }
    len1 = abs(vv1->u.integer.len);
    v1 = vv1->u.integer.data;
    v2 = vv2->u.integer.data;
    if (len1 < len2 || (len1 == len2 && v1[len1 - 1] < v2[len2 - 1])) {
        if (vv1 != rem) copy(vv1, rem);
        if (vv1 == vv) destroy(vv);
        vv->u.integer.val[0] = 0;
        vv->u.integer.len = 0;
        vv->u.integer.data = vv->u.integer.val;
        return;
    }
    negr = (vv1->u.integer.len < 0);
    neg = (negr != (vv2->u.integer.len < 0));
    if (len2 == 1) {
        ssize_t i;
        twodigits_t r = 0;
        digit_t n = v2[0];
        v = inew(vv, len1);
        for (i = len1; i--;) {
            digit_t h;
            r = (r << SHIFT) | v1[i];
            h = (digit_t)(r / n);
            v[i] = h;
            r -= (twodigits_t)h * n;
        }
        if (vv1 == vv) destroy(vv);
        if (vv1 == rem) destroy(rem);
        i = len1;
        while (i && !v[i - 1]) i--;
        if (i <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0]) && v != vv->u.integer.val) {
            memcpy(vv->u.integer.val, v, i * sizeof(digit_t));
            free(v);
            v = vv->u.integer.val;
        }
        vv->u.integer.data = v;
        vv->u.integer.len = neg ? -i : i;
        rem->u.integer.val[0] = r;
        rem->u.integer.data = rem->u.integer.val;
        rem->u.integer.len = (rem->u.integer.val[0] != 0);
    } else {
        typedef int32_t sdigit_t;
        typedef int64_t stwodigits_t;
        ssize_t i;
        int k, d;
        digit_t wm1, wm2, c, q, r, vtop, *v0, *vk, *w0, *ak, *a;
        twodigits_t vvv;
        sdigit_t zhi;
        stwodigits_t z;
        struct value_s tmp1, tmp2, tmp3;

        if (len1 + 1 < 1) err_msg_out_of_memory(); /* overflow */
        v0 = inew(&tmp1, len1 + 1);
        w0 = inew(&tmp2, len2);

        d = 0;
        while ((v2[len2 - 1] << d) <= MASK / 2) d++;

        if (d) {
            w0[0] = (v2[0] << d) & MASK;
            for (i = 1; i < len2; i++) w0[i] = ((v2[i] << d) | (v2[i - 1] >> (SHIFT - d))) & MASK;
            v0[0] = (v1[0] << d) & MASK;
            for (i = 1; i < len1; i++) v0[i] = ((v1[i] << d) | (v1[i - 1] >> (SHIFT - d))) & MASK;
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
            vtop = vk[len2];
            vvv = ((twodigits_t)vtop << SHIFT) | vk[len2 - 1];
            q = (digit_t)(vvv / wm1);
            r = (digit_t)(vvv - (twodigits_t)wm1 * q);
            while ((twodigits_t)wm2 * q > (((twodigits_t)r << SHIFT) | vk[len2 - 2])) {
                --q;
                r += wm1;
                if (r > MASK) break;
            }
            for (zhi = i = 0; i < len2; i++) {
                z = (sdigit_t)vk[i] + zhi - (stwodigits_t)q * (stwodigits_t)w0[i];
                vk[i] = z & MASK;
                if (z < 0) zhi = ~(~z >> SHIFT);
                else zhi = z >> SHIFT;
            }
            if ((sdigit_t)vtop + zhi < 0) {
                for (c = i = 0; i < len2; i++) {
                    c += vk[i] + w0[i];
                    vk[i] = c & MASK;
                    c >>= SHIFT;
                }
                --q;
            }
            *--ak = q;
        }
        if (w0 != tmp2.u.integer.val) free(w0);

        if (d) {
            for (i = 0; i < len2 - 1; i++) v0[i] = ((v0[i] >> d) | (v0[i + 1] << (SHIFT - d))) & MASK;
            v0[i] >>= d;
        } 

        if (vv1 == vv) destroy(vv);
        while (k && !a[k - 1]) k--;
        if (k <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0])) {
            memcpy(vv->u.integer.val, a, k * sizeof(digit_t));
            if (a != tmp3.u.integer.val) free(a);
            a = vv->u.integer.val;
        }
        vv->u.integer.data = a;
        vv->u.integer.len = neg ? -k : k;

        if (vv1 == rem) destroy(rem);
        while (len2 && !v0[len2 - 1]) len2--;
        if (len2 <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0])) {
            memcpy(rem->u.integer.val, v0, len2 * sizeof(digit_t));
            if (v0 != tmp1.u.integer.val) free(v0);
            v0 = rem->u.integer.val;
        }
        rem->u.integer.data = v0;
        rem->u.integer.len = len2;
    }
    if (negr) rem->u.integer.len = -rem->u.integer.len;
}

static MUST_CHECK value_t power(const value_t vv1, const value_t vv2) {
    int j, neg = 0;
    size_t i;
    value_t v = int_from_int(1);

    for (i = vv2->u.integer.len; i--;) {
        digit_t d = vv2->u.integer.data[i];
        for (j = 1 << (SHIFT - 1); j; j >>= 1) {
            imul(v, v, v);
            if (d & j) {
                imul(v, vv1, v);
                neg = 1;
            } else neg = 0;
        }
    }
    if (neg && vv1->u.integer.len < 0) v->u.integer.len = -v->u.integer.len;
    return v;
}

static void ilshift(const value_t vv1, const value_t vv2, uval_t s, value_t vv) {
    ssize_t i, len1, sz;
    int word, bit, neg;
    digit_t *v1, *v, *v2;

    word = s / SHIFT;
    bit = s % SHIFT;
    v1 = vv1->u.integer.data;
    len1 = abs(vv1->u.integer.len);
    neg = (vv1->u.integer.len < 0);
    sz = len1 + word + (bit > 0);
    v = inew(vv, sz);
    v2 = v + word;
    if (bit) {
        v2[len1] = 0;
        for (i = len1; i--;) {
            v2[i + 1] |= v1[i] >> (SHIFT - bit);
            v2[i] = (v1[i] << bit) & MASK;
        }
    } else if (len1) memmove(v2, v1, len1 * sizeof(digit_t));
    if (word) memset(v, 0, word * sizeof(digit_t));
    i = sz;
    while (i && !v[i - 1]) i--;
    if (i <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0]) && v != vv->u.integer.val) {
        memcpy(vv->u.integer.val, v, i * sizeof(digit_t));
        free(v);
        v = vv->u.integer.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->u.integer.data = v;
    vv->u.integer.len = neg ? -i : i;
}

static void irshift(value_t vv1, value_t vv2, uval_t s, value_t vv) {
    ssize_t i, sz;
    int word, bit, neg;
    digit_t *v1, *v;

    word = s / SHIFT;
    bit = s % SHIFT;
    neg = (vv1->u.integer.len < 0);
    if (neg) {
        isub(vv1, int_value[1], vv);
        vv1 = vv;
        sz = abs(vv->u.integer.len) - word;
    } else {
        sz = abs(vv1->u.integer.len) - word;
    }
    if (sz <= 0) {
        if (vv == vv1 || vv == vv2) destroy(vv);
        if (neg) {
            vv->u.integer.val[0] = 1;
            vv->u.integer.len = -1;
        } else {
            vv->u.integer.val[0] = 0;
            vv->u.integer.len = 0;
        }
        vv->u.integer.data = vv->u.integer.val;
        return;
    }
    v = inew(vv, sz);
    v1 = vv1->u.integer.data + word;
    if (bit) {
        for (i = 0; i < sz - 1; i++) {
            v[i] = v1[i] >> bit;
            v[i] |= (v1[i + 1] << (SHIFT - bit)) & MASK;
        }
        v[i] = v1[i] >> bit;
    } else if (sz) memmove(v, v1, sz * sizeof(digit_t));
    i = sz;
    if (neg) {
        if (vv == vv1 || vv == vv2) destroy(vv);
        vv->u.integer.data = v;
        vv->u.integer.len = i;
        iadd(int_value[1], vv, vv);
        vv->u.integer.len = -vv->u.integer.len;
        return;
    }
    while (i && !v[i - 1]) i--;
    if (i <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0]) && v != vv->u.integer.val) {
        memcpy(vv->u.integer.val, v, i * sizeof(digit_t));
        free(v);
        v = vv->u.integer.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->u.integer.data = v;
    vv->u.integer.len = i;
}

static void iand(value_t vv1, value_t vv2, value_t vv) {
    ssize_t i, len1, len2, sz;
    int neg1, neg2;
    digit_t *v1, *v2, *v;
    digit_t c;
    len1 = abs(vv1->u.integer.len);
    len2 = abs(vv2->u.integer.len);

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
        return;
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
            digit_t c1 = 1, c2 = 1;
            for (c = 1, i = 0; i < len1; i++) {
                c1 += v1[i] ^ MASK;
                c2 += (i < len2) ? (v2[i] ^ MASK) : MASK;
                c += (c1 & c2 & MASK) ^ MASK;
                v[i] = c & MASK;
                c = c > MASK;
                c1 = c1 > MASK;
                c2 = c2 > MASK;
            }
            v[i] = c;
        } else {
            digit_t c1 = 1;
            for (i = 0; i < len2; i++) {
                c1 += v1[i] ^ MASK;
                v[i] = c1 & v2[i] & MASK;
                c1 = c1 > MASK;
            }
        }
    } else {
        if (neg2) {
            digit_t c2 = 1;
            for (i = 0; i < len1; i++) {
                c2 += (i < len2) ? (v2[i] ^ MASK) : MASK;
                v[i] = c2 & v1[i] & MASK;
                c2 = c2 > MASK;
            }
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] & v2[i];
        }
    }
    while (sz && !v[sz - 1]) sz--;
    if (sz <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0]) && v != vv->u.integer.val) {
        memcpy(vv->u.integer.val, v, sz * sizeof(digit_t));
        free(v);
        v = vv->u.integer.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->u.integer.data = v;
    vv->u.integer.len = (neg1 & neg2) ? -sz : sz;
}

static void ior(value_t vv1, value_t vv2, value_t vv) {
    ssize_t i, len1, len2, sz;
    int neg1, neg2;
    digit_t *v1, *v2, *v;
    digit_t c;
    len1 = abs(vv1->u.integer.len);
    len2 = abs(vv2->u.integer.len);

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
        return;
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
            digit_t c1 = 1, c2 = 1;
            for (c = 1, i = 0; i < len2; i++) {
                c1 += v1[i] ^ MASK;
                c2 += v2[i] ^ MASK;
                c += ((c1 | c2) & MASK) ^ MASK;
                v[i] = c & MASK;
                c = c > MASK;
                c1 = c1 > MASK;
                c2 = c2 > MASK;
            }
        } else {
            digit_t c1 = 1;
            for (c = 1, i = 0; i < len1; i++) {
                c1 += v1[i] ^ MASK;
                c += (((i < len2) ? (c1 | v2[i]) : c1) & MASK) ^ MASK;
                v[i] = c & MASK;
                c = c > MASK;
                c1 = c1 > MASK;
            }
        }
        v[i] = c;
    } else {
        if (neg2) {
            digit_t c2 = 1;
            for (c = 1, i = 0; i < len2; i++) {
                c2 += v2[i] ^ MASK;
                c += ((c2 | v1[i]) & MASK) ^ MASK;
                v[i] = c & MASK;
                c = c > MASK;
                c2 = c2 > MASK;
            }
            v[i] = c;
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] | v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    }
    while (sz && !v[sz - 1]) sz--;
    if (sz <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0]) && vv->u.integer.val != v) {
        memcpy(vv->u.integer.val, v, sz * sizeof(digit_t));
        free(v);
        v = vv->u.integer.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->u.integer.data = v;
    vv->u.integer.len = (neg1 | neg2) ? -sz : sz;
}

static void ixor(value_t vv1, value_t vv2, value_t vv) {
    ssize_t i, len1, len2, sz;
    int neg1, neg2;
    digit_t *v1, *v2, *v;
    digit_t c;
    len1 = abs(vv1->u.integer.len);
    len2 = abs(vv2->u.integer.len);

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
        return;
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
            digit_t c1 = 1, c2 = 1;
            for (i = 0; i < len1; i++) {
                c1 += v1[i] ^ MASK;
                c2 += (i < len2) ? (v2[i] ^ MASK) : MASK;
                v[i] = (c1 ^ c2) & MASK;
                c1 = c1 > MASK;
                c2 = c2 > MASK;
            }
        } else {
            digit_t c1 = 1;
            for (c = 1, i = 0; i < len1; i++) {
                c1 += v1[i] ^ MASK;
                c += (((i < len2) ? (c1 ^ v2[i]) : c1) & MASK) ^ MASK;
                v[i] = c & MASK;
                c = c > MASK;
                c1 = c1 > MASK;
            }
            v[i] = c;
        }
    } else {
        if (neg2) {
            digit_t c2 = 1;
            for (c = 1, i = 0; i < len1; i++) {
                c2 += (i < len2) ? (v2[i] ^ MASK) : MASK;
                c += ((c2 ^ v1[i]) & MASK) ^ MASK;
                v[i] = c & MASK;
                c = c > MASK;
                c2 = c2 > MASK;
            }
            v[i] = c;
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    }
    while (sz && !v[sz - 1]) sz--;
    if (sz <= (ssize_t)sizeof(vv->u.integer.val)/(ssize_t)sizeof(vv->u.integer.val[0]) && vv->u.integer.val != v) {
        memcpy(vv->u.integer.val, v, sz * sizeof(digit_t));
        free(v);
        v = vv->u.integer.val;
    }
    if (vv == vv1 || vv == vv2) destroy(vv);
    vv->u.integer.data = v;
    vv->u.integer.len = (neg1 ^ neg2) ? -sz : sz;
}

static int icmp(const value_t vv1, const value_t vv2) {
    ssize_t i;
    i = vv1->u.integer.len - vv2->u.integer.len;
    if (i) return i;
    i = abs(vv1->u.integer.len);
    while (i && vv1->u.integer.data[i - 1] == vv2->u.integer.data[i - 1]) i--;
    if (!i) return 0;
    i = vv1->u.integer.data[i - 1] - vv2->u.integer.data[i - 1];
    return (vv1->u.integer.len < 0) ? -i : i;
}

MUST_CHECK value_t int_from_int(int i) {
    value_t v = val_alloc();
    v->obj = INT_OBJ;
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

MUST_CHECK value_t int_from_uval(uval_t i) {
    value_t v = val_alloc();
    v->obj = INT_OBJ;
    v->u.integer.data = v->u.integer.val;
    if (i > MASK) {
        v->u.integer.val[0] = i & MASK;
        v->u.integer.val[1] = i >> SHIFT;
        v->u.integer.len = 2;
        return v;
    }
    v->u.integer.val[0] = i;
    v->u.integer.len = (i != 0);
    return v;
}

MUST_CHECK value_t int_from_ival(ival_t i) {
    value_t v = val_alloc();
    v->obj = INT_OBJ;
    v->u.integer.data = v->u.integer.val;
    if (i < 0) {
        if (i < -MASK) {
            v->u.integer.val[0] = (-i) & MASK;
            v->u.integer.val[1] = (-i) >> SHIFT;
            v->u.integer.len = -2;
            return v;
        }
        v->u.integer.val[0] = -i;
        v->u.integer.len = -1;
        return v;
    }
    if (i > MASK) {
        v->u.integer.val[0] = i & MASK;
        v->u.integer.val[1] = i >> SHIFT;
        v->u.integer.len = 2;
        return v;
    }
    v->u.integer.val[0] = i;
    v->u.integer.len = (i != 0);
    return v;
}

MUST_CHECK value_t int_from_double(double f, linepos_t epoint) {
    int neg, expo;
    double frac;
    ssize_t sz;
    digit_t *d;
    value_t v;

    if (f == HUGE_VAL) {
        v = val_alloc();
        v->obj = ERROR_OBJ;
        v->u.error.num = ERROR______CANT_INT;
        v->u.error.epoint = *epoint;
        v->u.error.u.objname = FLOAT_OBJ->name;
        return v;
    }
    neg = (f < 0.0);
    if (neg) f = -f;

    frac = frexp(f, &expo);
    if (expo < 0) {
        return val_reference(int_value[0]);
    }
    sz = (expo - 1) / SHIFT + 1;

    v = val_alloc();
    d = inew(v, sz);
    v->obj = INT_OBJ;
    v->u.integer.len = neg ? -sz : +sz;
    v->u.integer.data = d;

    frac = ldexp(frac, (expo - 1) % SHIFT + 1);

    while (sz--) {
        digit_t dg = (digit_t)frac & MASK;
        d[sz] = dg;
        frac = ldexp(frac - (double)dg, SHIFT);
    }
    return v;
}

MUST_CHECK value_t int_from_bytes(const value_t v1) {
    uval_t uv = 0;
    int bits = 0;
    ssize_t j = 0, sz;
    size_t i = 0;
    digit_t *d;
    const uint8_t *b;
    value_t v;

    if (!v1->u.bytes.len) {
        return val_reference(int_value[0]);
    }
    sz = (v1->u.bytes.len * 8 + SHIFT - 1) / SHIFT;
    if (v1->u.bytes.len > (SSIZE_MAX - SHIFT + 1) / 8) err_msg_out_of_memory(); /* overflow */
    v = val_alloc();
    d = inew(v, sz);

    b = v1->u.bytes.data;
    while (v1->u.bytes.len > i) {
        uv |= b[i] << bits;
        bits += 8 * sizeof(b[0]);
        if (bits >= SHIFT) {
            if (j >= sz) err_msg_out_of_memory();
            d[j++] = uv & MASK;
            bits -= SHIFT;
            uv = b[i] >> (8 * sizeof(b[0]) - bits);
        }
        i++;
    }
    if (bits) {
        if (j >= sz) err_msg_out_of_memory();
        d[j++] = uv & MASK;
    }
    sz = j;

    while (sz && !d[sz - 1]) sz--;
    if (sz <= (ssize_t)sizeof(v->u.integer.val)/(ssize_t)sizeof(v->u.integer.val[0]) && v->u.integer.val != d) {
        memcpy(v->u.integer.val, d, sz * sizeof(digit_t));
        free(d);
        d = v->u.integer.val;
    }
    v->obj = INT_OBJ;
    v->u.integer.data = d;
    v->u.integer.len = sz;
    return v;
}

MUST_CHECK value_t int_from_bits(const value_t v1) {
    uval_t uv = 0;
    int bits = 0;
    int inv;
    ssize_t j = 0, sz;
    size_t i = 0;
    digit_t *d;
    const bdigit_t *b;
    value_t v;

    if (!v1->u.bits.len) {
        if (v1->u.bits.inv) {
            return int_from_int(-1);
        }
        return val_reference(int_value[0]);
    }

    inv = v1->u.bits.inv;
    sz = (v1->u.bits.len * 8 * sizeof(bdigit_t) + SHIFT - 1 + (inv && (bdigit_t)~0 == v1->u.bits.data[v1->u.bits.len - 1])) / SHIFT;
    if (v1->u.bits.len > (SSIZE_MAX - SHIFT) / 8 / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
    v = val_alloc();
    d = inew(v, sz);

    b = v1->u.bits.data;
    if (inv) {
        int c = 1;
        while (v1->u.bits.len > i) {
            uv |= (bdigit_t)(b[i] + c) << bits;
            bits += 8 * sizeof(bdigit_t);
            if (bits >= SHIFT) {
                if (j >= sz) err_msg_out_of_memory();
                d[j++] = uv & MASK;
                bits -= SHIFT;
                uv = (bdigit_t)(b[i] + c) >> (8 * sizeof(bdigit_t) - bits);
            }
            if (b[i] < ((bdigit_t)~0)) c = 0;
            i++;
        }
        if (c) {
            uv |= 1 << bits;
            bits++;
            if (bits >= SHIFT) {
                if (j >= sz) err_msg_out_of_memory();
                d[j++] = uv & MASK;
                uv >>= SHIFT;
                bits -= SHIFT;
            }
        }
    } else {
        while (v1->u.bits.len > i) {
            uv |= b[i] << bits;
            bits += 8 * sizeof(bdigit_t);
            if (bits >= SHIFT) {
                if (j >= sz) err_msg_out_of_memory();
                d[j++] = uv & MASK;
                bits -= SHIFT;
                uv = b[i] >> (8 * sizeof(bdigit_t) - bits);
            }
            i++;
        }
    }
    if (bits) {
        if (j >= sz) err_msg_out_of_memory();
        d[j++] = uv & MASK;
    }
    sz = j;

    while (sz && !d[sz - 1]) sz--;
    if (sz <= (ssize_t)sizeof(v->u.integer.val)/(ssize_t)sizeof(v->u.integer.val[0]) && v->u.integer.val != d) {
        memcpy(v->u.integer.val, d, sz * sizeof(digit_t));
        free(d);
        d = v->u.integer.val;
    }
    v->obj = INT_OBJ;
    v->u.integer.data = d;
    v->u.integer.len = inv ? -sz : sz;
    return v;
}

MUST_CHECK value_t int_from_str(const value_t v1, linepos_t epoint) {
    int ch;
    value_t v;

    if (actual_encoding) {
        uval_t uv = 0;
        int bits = 0;
        ssize_t j = 0, sz, osz;
        digit_t *d;

        if (!v1->u.str.len) {
            return val_reference(int_value[0]);
        }

        sz = (v1->u.str.len * 8 + SHIFT - 1) / SHIFT;
        if (v1->u.str.len > (SSIZE_MAX - SHIFT + 1) / 8) err_msg_out_of_memory(); /* overflow */
        v = val_alloc();
        d = inew(v, sz);

        encode_string_init(v1, epoint);
        while ((ch = encode_string()) != EOF) {
            uv |= (uint8_t)ch << bits;
            bits += 8;
            if (bits >= SHIFT) {
                if (j >= sz) {
                    if (v->u.integer.val == d) {
                        sz = 16 / sizeof(digit_t);
                        d = (digit_t *)malloc(sz * sizeof(digit_t));
                        memcpy(d, v->u.bytes.val, j * sizeof(digit_t));
                    } else {
                        sz += 1024 / sizeof(digit_t);
                        if (sz < 1024 / (ssize_t)sizeof(digit_t)) err_msg_out_of_memory(); /* overflow */
                        d = (digit_t *)realloc(d, sz * sizeof(digit_t));
                    }
                    if (!d) err_msg_out_of_memory();
                }
                d[j++] = uv & MASK;
                bits -= SHIFT;
                uv = (uint8_t)ch >> (8 - bits);
            }
        }
        if (bits) {
            if (j >= sz) {
                sz++;
                if (sz < 1) err_msg_out_of_memory(); /* overflow */
                if (v->u.integer.val == d) {
                    d = (digit_t *)malloc(sz * sizeof(digit_t));
                    memcpy(d, v->u.bytes.val, j * sizeof(digit_t));
                } else d = (digit_t *)realloc(d, sz * sizeof(digit_t));
                if (!d) err_msg_out_of_memory();
            }
            d[j++] = uv & MASK;
        }
        osz = j;

        while (osz && !d[osz - 1]) osz--;
        if (v->u.integer.val != d) {
            if (osz <= (ssize_t)sizeof(v->u.integer.val)/(ssize_t)sizeof(v->u.integer.val[0])) {
                memcpy(v->u.integer.val, d, osz * sizeof(digit_t));
                free(d);
                d = v->u.integer.val;
            } else if (osz < sz) {
                d = (digit_t *)realloc(d, osz * sizeof(digit_t));
                if (!d) err_msg_out_of_memory();
            }
        }
        v->obj = INT_OBJ;
        v->u.integer.data = d;
        v->u.integer.len = osz;
        return v;
    } 
    if (v1->u.str.chars == 1) {
        uint32_t ch2 = v1->u.str.data[0];
        if (ch2 & 0x80) utf8in(v1->u.str.data, &ch2);
        return int_from_uval(ch2);
    } 
    v = val_alloc();
    v->obj = ERROR_OBJ;
    v->u.error.num = ERROR_BIG_STRING_CO;
    v->u.error.epoint = *epoint;
    return v;
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
        if (val) {
            v = val_alloc();
            v->obj = INT_OBJ;
            v->u.integer.val[0] = val;
            v->u.integer.data = v->u.integer.val;
            v->u.integer.len = 1;
            return v;
        }
        return val_reference(int_value[0]);
    }
    sz = (double)i * 0.11073093649624542178511177326072356663644313812255859375 + 1;
    l = i;

    v = val_alloc();
    v->obj = INT_OBJ;
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
            a += (twodigits_t)*d2 * mul;
            *d2++ = a & MASK;
            a >>= SHIFT;
        }
        if (a) {
            if (end2 >= &d[sz]) {
                sz++;
                if (sz > (ssize_t)sizeof(v->u.integer.val)/(ssize_t)sizeof(v->u.integer.val[0])) {
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
    while (sz && !d[sz - 1]) sz--;
    if (sz <= (ssize_t)sizeof(v->u.integer.val)/(ssize_t)sizeof(v->u.integer.val[0]) && v->u.integer.val != d) {
        memcpy(v->u.integer.val, d, sz * sizeof(digit_t));
        free(d);
        d = v->u.integer.val;
    }
    v->u.integer.data = d;
    v->u.integer.len = sz;
    *ln = l;
    return v;
}

static MUST_CHECK value_t calc2_int(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2, v;
    struct value_s tmp;
    value_t err;
    ival_t shift;
    int i;
    switch (op->op->u.oper.op) {
    case O_CMP: 
        i = icmp(v1, v2);
        if (i < 0) return int_from_int(-1);
        return val_reference(int_value[i > 0]);
    case O_EQ: return truth_reference(icmp(v1, v2) == 0);
    case O_NE: return truth_reference(icmp(v1, v2) != 0);
    case O_LT: return truth_reference(icmp(v1, v2) < 0);
    case O_LE: return truth_reference(icmp(v1, v2) <= 0);
    case O_GT: return truth_reference(icmp(v1, v2) > 0);
    case O_GE: return truth_reference(icmp(v1, v2) >= 0);
    case O_ADD: 
        v = val_alloc();
        v->obj = INT_OBJ; 
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
        v = val_alloc();
        v->obj = INT_OBJ; 
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
        v = val_alloc();
        v->obj = INT_OBJ; 
        if ((v1->u.integer.len ^ v2->u.integer.len) < 0) {
            imul(v1, v2, v);
            v->u.integer.len = -v->u.integer.len;
        } else imul(v1, v2, v);
        return v;
    case O_DIV:
        v = val_alloc();
        i = (v2->u.integer.len < 0);
        v->obj = INT_OBJ; 
        idivrem(v1, v2, v, &tmp);
        if (v->obj == ERROR_OBJ) {
            v->u.error.epoint = *op->epoint2;
            return v;
        }
        if ((tmp.u.integer.len < 0) ^ i) {
            if (v->u.integer.len < 0) {
                iadd(v, int_value[1], v);
                v->u.integer.len = -v->u.integer.len;
            } else isub(v, int_value[1], v);
        }
        if (tmp.u.integer.data != tmp.u.integer.val) free(tmp.u.integer.data);
        return v;
    case O_MOD:
        v = val_alloc();
        v->obj = INT_OBJ; 
        idivrem(v1, v2, &tmp, v);
        if (v->obj == ERROR_OBJ) {
            v->u.error.epoint = *op->epoint2;
            return v;
        }
        if ((v->u.integer.len < 0) ^ (v2->u.integer.len < 0)) {
            if (v->u.integer.len < 0) {
                if (v2->u.integer.len < 0) {
                    iadd(v, v2, v);
                    v->u.integer.len = -v->u.integer.len;
                } else isub(v2, v, v);
            } else {
                if (v2->u.integer.len < 0) isub(v, v2, v);
                else iadd(v, v2, v);
            }
        }
        if (tmp.u.integer.data != tmp.u.integer.val) free(tmp.u.integer.data);
        return v;
    case O_EXP:
        if (v2->u.integer.len < 0) {
            double d1, d2;
            err = real(v1, &d1, op->epoint);
            if (err) return err;
            err = real(v2, &d2, op->epoint);
            if (err) return err;
            return calc2_double(op, d1, d2);
        }
        return power(v1, v2);
    case O_LSHIFT:
        err = ival(v2, &shift, 8*sizeof(ival_t), op->epoint2);
        if (err) return err;
        v = val_alloc();
        v->obj = INT_OBJ;
        if (shift < 0) irshift(v1, v2, -shift, v);
        else ilshift(v1, v2, shift, v);
        return v;
    case O_RSHIFT:
        err = ival(v2, &shift, 8*sizeof(ival_t), op->epoint2);
        if (err) return err;
        v = val_alloc();
        v->obj = INT_OBJ;
        if (shift < 0) ilshift(v1, v2, -shift, v);
        else irshift(v1, v2, shift, v);
        return v;
    case O_AND:
        v = val_alloc();
        v->obj = INT_OBJ; 
        iand(v1, v2, v);
        return v;
    case O_OR:
        v = val_alloc();
        v->obj = INT_OBJ; 
        ior(v1, v2, v);
        return v;
    case O_XOR:
        v = val_alloc();
        v->obj = INT_OBJ; 
        ixor(v1, v2, v);
        return v;
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
    default: 
        if (op->op != &o_MEMBER) {
            return v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    value_t tmp, ret, v1 = op->v1;
    switch (v1->obj->type) {
    case T_INT: return calc2_int(op);
    case T_BOOL:
        tmp = val_reference(int_value[v1->u.boolean]);
        op->v1 = tmp;
        ret = calc2_int(op);
        val_destroy(tmp);
        op->v1 = v1;
        return ret;
    default:
        if (op->op != &o_IN) {
            return v1->obj->calc2(op);
        }
    }
    return obj_oper_error(op);
}

void intobj_init(void) {
    obj_init(&obj, T_INT, "<int>");
    obj.destroy = destroy;
    obj.copy = copy;
    obj.same = same;
    obj.truth = truth;
    obj.hash = hash;
    obj.repr = repr;
    obj.ival = ival;
    obj.uval = uval;
    obj.real = real;
    obj.sign = sign;
    obj.abs = absolute;
    obj.integer = integer;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
