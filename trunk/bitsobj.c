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
#include "bitsobj.h"
#include "eval.h"
#include "unicode.h"
#include "encoding.h"
#include "boolobj.h"

#define SHIFT (8 * sizeof(bdigit_t))

static struct obj_s obj;

obj_t BITS_OBJ = &obj;

static void destroy(value_t v1) {
    if (v1->u.bits.val != v1->u.bits.data) free(v1->u.bits.data);
}

static bdigit_t *bnew(value_t v, size_t len) {
    if (len > sizeof(v->u.bits.val)/sizeof(v->u.bits.val[0])) {
        bdigit_t *s = (bdigit_t *)malloc(len * sizeof(bdigit_t));
        if (!s || len > SIZE_MAX / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
        return s;
    }
    return v->u.bits.val;
}

static MUST_CHECK value_t invert(const value_t v1) {
    value_t v = val_alloc(BITS_OBJ);
    v->u.bits.bits = v1->u.bits.bits;
    v->u.bits.inv = !v1->u.bits.inv;
    v->u.bits.len = v1->u.bits.len;
    if (v1->u.bits.len) {
        v->u.bits.data = bnew(v, v->u.bits.len);
        memcpy(v->u.bits.data, v1->u.bits.data, v->u.bits.len * sizeof(bdigit_t));
    } else {
        v->u.bits.val[0] = 0;
        v->u.bits.data = v->u.bits.val;
    }
    return v;
}

static MUST_CHECK value_t normalize(value_t v, bdigit_t *d, size_t sz) {
    while (sz && !d[sz - 1]) sz--;
    if (v->u.bits.val != d && sz <= sizeof(v->u.bits.val)/sizeof(v->u.bits.val[0])) {
        memcpy(v->u.bits.val, d, sz);
        free(d);
        d = v->u.bits.val;
    }
    v->u.bits.data = d;
    v->u.bits.len = sz;
    return v;
}

static int same(const value_t v1, const value_t v2) {
    if (v2->obj != BITS_OBJ || v1->u.bits.len != v2->u.bits.len || v1->u.bits.bits != v2->u.bits.bits || v1->u.bits.inv != v2->u.bits.inv) return 0;
    return !memcmp(v1->u.bits.data, v2->u.bits.data, v1->u.bits.len * sizeof(bdigit_t));
}

static MUST_CHECK value_t truth(const value_t v1, enum truth_e type, linepos_t epoint) {
    value_t v;
    size_t i;
    bdigit_t b;
    switch (type) {
    case TRUTH_ALL:
        for (i = 0; i < v1->u.bits.bits / SHIFT; i++) {
            b = (i >= v1->u.bits.len) ? 0 : v1->u.bits.data[i];
            if (!v1->u.bits.inv) b = ~b;
            if (b) return val_reference(false_value);
        }
        b = (i >= v1->u.bits.len) ? 0 : v1->u.bits.data[i];
        if (!v1->u.bits.inv) b = ~b;
        b <<= SHIFT - v1->u.bits.bits % SHIFT;
        return truth_reference(!b);
    case TRUTH_ANY:
        for (i = 0; i < v1->u.bits.bits / SHIFT; i++) {
            b = (i >= v1->u.bits.len) ? 0 : v1->u.bits.data[i];
            if (v1->u.bits.inv) b = ~b;
            if (b) return val_reference(true_value);
        }
        b = (i >= v1->u.bits.len) ? 0 : v1->u.bits.data[i];
        if (v1->u.bits.inv) b = ~b;
        b <<= SHIFT - v1->u.bits.bits % SHIFT;
        return truth_reference(!!b);
    case TRUTH_BOOL:
        return truth_reference(!!v1->u.bits.len || v1->u.bits.inv);
    default:
        v = new_error_obj(ERROR_____CANT_BOOL, epoint);
        v->u.error.u.objname = v1->obj->name;
        return v;
    }
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t UNUSED(epoint)) {
    size_t len, i, len2;
    uint8_t *s;
    value_t v = val_alloc(STR_OBJ);

    len2 = v1->u.bits.bits;
    if (len2 & 3) {
        len = 1 + v1->u.bits.inv;
        len += len2;
        s = str_create_elements(v, len);

        len = 0;
        if (v1->u.bits.inv) s[len++] = '~';
        s[len++] = '%';
        for (i = len2; i--;) {
            size_t j = i / SHIFT;
            s[len++] = (j >= v1->u.bits.len) ? 0x30 : 0x30 | ((v1->u.bits.data[j] >> (i & (SHIFT - 1))) & 1);
        }
    } else {
        static const char *hex = "0123456789abcdef";
        len2 /= 4;
        len = 1 + v1->u.bits.inv;
        len += len2;
        s = str_create_elements(v, len);

        len = 0;
        if (v1->u.bits.inv) s[len++] = '~';
        s[len++] = '$';
        for (i = len2; i--;) {
            size_t j = i / (2 * sizeof(bdigit_t));
            s[len++] = (j >= v1->u.bits.len) ? 0x30 : (uint8_t)hex[(v1->u.bits.data[j] >> ((i & (2 * sizeof(bdigit_t) - 1)) * 4)) & 15];
        }
    }
    v->u.str.len = len;
    v->u.str.chars = len;
    v->u.str.data = s;
    return v;
}

static MUST_CHECK value_t hash(const value_t v1, int *hs, linepos_t UNUSED(epoint)) {
    size_t l = v1->u.bits.len;
    unsigned int h;

    switch (l) {
    case 0: *hs = (-v1->u.bits.inv) & ((~(unsigned int)0) >> 1); return NULL;
    case 1: *hs = (v1->u.bits.inv ? ~v1->u.bits.data[0] : v1->u.bits.data[0]) & ((~(unsigned int)0) >> 1); return NULL;
    }
    if (v1->u.bits.inv) {
        h = -1;
        while (l--) {
            h -= v1->u.integer.val[l];
        }
    } else {
        h = 0;
        while (l--) {
            h += v1->u.integer.val[l];
        }
    }
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

static MUST_CHECK value_t ival(const value_t v1, ival_t *iv, int bits, linepos_t epoint) {
    value_t v;
    switch (v1->u.bits.len) {
    case 1: *iv = v1->u.bits.data[0];
            if (bits < (int)SHIFT && (uval_t)*iv >> bits) break;
            if (v1->u.bits.inv) *iv = ~*iv;
            return NULL;
    case 0: *iv = v1->u.bits.inv ? ~(ival_t)0 : 0; return NULL;
    default: break;
    }
    v = new_error_obj(ERROR_____CANT_IVAL, epoint);
    v->u.error.u.bits = bits;
    return v;
}

static MUST_CHECK value_t uval(const value_t v1, uval_t *uv, int bits, linepos_t epoint) {
    value_t v;
    switch (v1->u.bits.len) {
    case 1: *uv = v1->u.bits.data[0];
            if (bits < (int)SHIFT && *uv >> bits) break;
            if (v1->u.bits.inv) *uv = ~*uv;
            return NULL;
    case 0: *uv = v1->u.bits.inv ? ~(uval_t)0 : 0; return NULL;
    default: break;
    }
    v = new_error_obj(ERROR_____CANT_UVAL, epoint);
    v->u.error.u.bits = bits;
    return v;
}

static MUST_CHECK value_t real(const value_t v1, double *r, linepos_t epoint) {
    size_t i, len1 = v1->u.bits.len;
    double d = -v1->u.bits.inv;
    for (i = 0; i < len1; i++) {
        if (v1->u.bits.inv) d -= ldexp((double)v1->u.bits.data[i], i * SHIFT);
        else d += ldexp((double)v1->u.bits.data[i], i * SHIFT);
        if (d == HUGE_VAL) {
            value_t v = new_error_obj(ERROR_____CANT_REAL, epoint);
            v->u.error.u.objname = v1->obj->name;
            return v;
        }
    }
    *r = d;
    return NULL;
}

static MUST_CHECK value_t sign(const value_t v1, linepos_t UNUSED(epoint)) {
    if (!v1->u.bits.len) return val_reference(int_value[0]);
    if (v1->u.bits.inv) return int_from_int(-1);
    return val_reference(int_value[1]);
}

static MUST_CHECK value_t absolute(const value_t v1, linepos_t epoint) {
    value_t tmp, ret;
    tmp = int_from_bits(v1);
    ret = tmp->obj->abs(tmp, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK value_t integer(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_bits(v1);
}

static MUST_CHECK value_t len(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_uval(v1->u.bits.bits);
}

static MUST_CHECK value_t bits_from_bool(int i) {
    value_t v = val_alloc(BITS_OBJ);
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = i;
    v->u.bits.len = (i != 0);
    v->u.bits.inv = 0;
    v->u.bits.bits = 1;
    return v;
}

MUST_CHECK value_t bits_from_bools(int i, int j) {
    value_t v = val_alloc(BITS_OBJ);
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = (i << 1) | j;
    v->u.bits.len = i | j;
    v->u.bits.inv = 0;
    v->u.bits.bits = 2;
    return v;
}

MUST_CHECK value_t bits_from_u8(uint8_t i) {
    value_t v = val_alloc(BITS_OBJ);
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = i;
    v->u.bits.len = (i != 0);
    v->u.bits.inv = 0;
    v->u.bits.bits = 8;
    return v;
}

MUST_CHECK value_t bits_from_u16(uint16_t i) {
    value_t v = val_alloc(BITS_OBJ);
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = i;
    v->u.bits.len = (i != 0);
    v->u.bits.inv = 0;
    v->u.bits.bits = 16;
    return v;
}

static MUST_CHECK value_t bits_from_u24(uint32_t i) {
    value_t v = val_alloc(BITS_OBJ);
    v->u.bits.data = v->u.bits.val;
    v->u.bits.val[0] = i;
    v->u.bits.len = (i != 0);
    v->u.bits.inv = 0;
    v->u.bits.bits = 24;
    return v;
}

MUST_CHECK value_t bits_from_hexstr(const uint8_t *s, size_t *ln) {
    size_t i = 0, j, sz;
    int bits;
    bdigit_t *d, uv;
    value_t v;

    while ((s[i] ^ 0x30) < 10 || (uint8_t)((s[i] | 0x20) - 0x61) < 6) i++;
    *ln = i;
    if (!i) {
        return val_reference(null_bits);
    }

    v = val_alloc(BITS_OBJ);
    v->u.bits.inv = 0;
    v->u.bits.bits = i * 4;

    sz = i / (2 * sizeof(bdigit_t));
    if (i % (2 * sizeof(bdigit_t))) sz++;
    d = bnew(v, sz);

    uv = bits = j = 0;
    while (i--) {
        if (s[i] < 0x40) uv |= (s[i] & 15) << bits;
        else uv |= ((s[i] & 7) + 9) << bits;
        if (bits == SHIFT - 4) {
            d[j++] = uv;
            bits = uv = 0;
        } else bits += 4;
    }
    if (bits) d[j] = uv;

    return normalize(v, d, sz);
}

MUST_CHECK value_t bits_from_binstr(const uint8_t *s, size_t *ln) {
    size_t i = 0, j, sz;
    int bits;
    bdigit_t *d, uv;
    value_t v;

    while ((s[i] & 0xfe) == 0x30) i++;
    *ln = i;
    if (!i) {
        return val_reference(null_bits);
    }

    v = val_alloc(BITS_OBJ);
    v->u.bits.inv = 0;
    v->u.bits.bits = i;

    sz = i / SHIFT;
    if (i % SHIFT) sz++;
    d = bnew(v, sz);

    uv = bits = j = 0;
    while (i--) {
        if (s[i] == 0x31) uv |= 1 << bits;
        if (bits == SHIFT - 1) {
            d[j++] = uv;
            bits = uv = 0;
        } else bits++;
    }
    if (bits) d[j] = uv;

    return normalize(v, d, sz);
}

MUST_CHECK value_t bits_from_str(const value_t v1, linepos_t epoint) {
    int ch;
    value_t v;

    if (actual_encoding) {
        int bits;
        size_t j, sz, osz;
        bdigit_t *d, uv;

        if (!v1->u.str.len) {
            return val_reference(null_bits);
        }

        if (v1->u.str.len <= sizeof(v->u.bits.val)) sz = sizeof(v->u.bits.val) / sizeof(v->u.bits.val[0]);
        else {
            sz = v1->u.str.len / sizeof(bdigit_t);
            if (v1->u.str.len % sizeof(bdigit_t)) sz++;
        }
        v = val_alloc(BITS_OBJ);
        d = bnew(v, sz);

        uv = bits = j = 0;
        encode_string_init(v1, epoint);
        while ((ch = encode_string()) != EOF) {
            uv |= (uint8_t)ch << bits;
            if (bits == SHIFT - 8) {
                if (j >= sz) {
                    if (v->u.bits.val == d) {
                        sz = 16 / sizeof(bdigit_t);
                        d = (bdigit_t *)malloc(sz * sizeof(bdigit_t));
                        memcpy(d, v->u.bytes.val, j * sizeof(bdigit_t));
                    } else {
                        sz += 1024 / sizeof(bdigit_t);
                        if (sz < 1024 / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
                        d = (bdigit_t *)realloc(d, sz * sizeof(bdigit_t));
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
                if (v->u.bits.val == d) {
                    d = (bdigit_t *)malloc(sz * sizeof(bdigit_t));
                    memcpy(d, v->u.bytes.val, j * sizeof(bdigit_t));
                } else d = (bdigit_t *)realloc(d, sz * sizeof(bdigit_t));
                if (!d) err_msg_out_of_memory();
            }
            d[j] = uv;
            osz = j + 1;
        } else osz = j;

        while (osz && !d[osz - 1]) osz--;
        if (v->u.bits.val != d) {
            if (osz <= sizeof(v->u.bits.val)/sizeof(v->u.bits.val[0])) {
                memcpy(v->u.bits.val, d, osz * sizeof(bdigit_t));
                free(d);
                d = v->u.bits.val;
            } else if (osz < sz) {
                d = (bdigit_t *)realloc(d, osz * sizeof(bdigit_t));
                if (!d) err_msg_out_of_memory();
            }
        }
        v->u.bits.data = d;
        v->u.bits.len = osz;
        v->u.bits.inv = 0;
        v->u.bits.bits = j * SHIFT + bits;
        return v;
    }
    if (v1->u.str.chars == 1) {
        uint32_t ch2 = v1->u.str.data[0];
        if (ch2 & 0x80) utf8in(v1->u.str.data, &ch2);
        return bits_from_u24(ch2);
    }
    return new_error_obj(ERROR_BIG_STRING_CO, epoint);
}

MUST_CHECK value_t bits_from_bytes(const value_t v1) {
    int bits;
    size_t i, j, sz;
    bdigit_t *d, uv;
    value_t v;

    i = v1->u.bytes.len;
    if (!i) {
        return val_reference(null_bits);
    }

    v = val_alloc(BITS_OBJ);
    v->u.bits.inv = 0;
    v->u.bits.bits = i * 8;

    sz = i / sizeof(bdigit_t);
    if (i % sizeof(bdigit_t)) sz++;
    d = bnew(v, sz);

    uv = bits = j = i = 0;
    while (v1->u.bytes.len > i) {
        uv |= v1->u.bytes.data[i++] << bits;
        if (bits == SHIFT - 8) {
            d[j++] = uv;
            bits = uv = 0;
        } else bits += 8;
    }
    if (bits) d[j] = uv;

    return normalize(v, d, sz);
}

static MUST_CHECK value_t calc1(oper_t op) {
    value_t v1 = op->v1, v;
    value_t tmp;
    bdigit_t uv;
    switch (op->op->u.oper.op) {
    case O_BANK:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        return bits_from_u8(uv >> 16);
    case O_HIGHER:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        return bits_from_u8(uv >> 8);
    case O_LOWER:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        return bits_from_u8(uv);
    case O_HWORD:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        return bits_from_u16(uv >> 8);
    case O_WORD:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        return bits_from_u16(uv);
    case O_BSWORD:
        uv = v1->u.bits.len > 0 ? v1->u.bits.data[0] : 0;
        if (v1->u.bits.inv) uv = ~uv;
        return bits_from_u16((uint8_t)(uv >> 8) | (uint16_t)(uv << 8));
    case O_INV: return invert(v1);
    case O_NEG:
    case O_POS:
    case O_STRING:
        tmp = int_from_bits(v1);
        op->v1 = tmp;
        v = tmp->obj->calc1(op);
        val_destroy(tmp);
        op->v1 = v1;
        return v;
    default:
        break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t and_(value_t vv1, value_t vv2) {
    size_t blen1, blen2;
    size_t i, len1, len2, sz;
    int neg1, neg2;
    bdigit_t *v1, *v2, *v;
    value_t vv = val_alloc(BITS_OBJ);
    blen1 = vv1->u.bits.bits;
    blen2 = vv2->u.bits.bits;
    if (blen1 < blen2) {
        vv->u.bits.bits = vv1->u.bits.inv ? blen2 : blen1;
    } else {
        vv->u.bits.bits = vv2->u.bits.inv ? blen1 : blen2;
    }
    len1 = vv1->u.bits.len;
    len2 = vv2->u.bits.len;

    if (len1 <= 1 && len2 <= 1) {
        bdigit_t c;
        neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;
        if (neg1) {
            if (neg2) {
                c = vv1->u.bits.data[0] | vv2->u.bits.data[0];
            } else {
                c = ~vv1->u.bits.data[0] & vv2->u.bits.data[0];
            }
        } else {
            c = vv1->u.bits.data[0] & (neg2 ? ~vv2->u.bits.data[0] : vv2->u.bits.data[0]);
        }
        v = vv->u.bits.val;
        v[0] = c;
        vv->u.bits.data = v;
        vv->u.bits.len = (c != 0);
        vv->u.bits.inv = neg1 & neg2;
        return vv;
    }
    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->u.bits.data; v2 = vv2->u.bits.data;
    neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;

    sz = neg2 ? len1 : len2;
    v = bnew(vv, sz);

    if (neg1) {
        if (neg2) {
            for (i = 0; i < len2; i++) v[i] = v1[i] | v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        } else {
            for (i = 0; i < len2; i++) v[i] = ~v1[i] & v2[i];
        }
    } else {
        if (neg2) {
            for (i = 0; i < len2; i++) v[i] = v1[i] & ~v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] & v2[i];
        }
    }

    vv->u.bits.inv = neg1 & neg2;
    return normalize(vv, v, sz);
}

static MUST_CHECK value_t or_(value_t vv1, value_t vv2) {
    size_t blen1, blen2;
    size_t i, len1, len2, sz;
    int neg1, neg2;
    bdigit_t *v1, *v2, *v;
    value_t vv = val_alloc(BITS_OBJ);
    blen1 = vv1->u.bits.bits;
    blen2 = vv2->u.bits.bits;
    if (blen1 < blen2) {
        vv->u.bits.bits = vv1->u.bits.inv ? blen1 : blen2;
    } else {
        vv->u.bits.bits = vv2->u.bits.inv ? blen2 : blen1;
    }
    len1 = vv1->u.bits.len;
    len2 = vv2->u.bits.len;

    if (len1 <= 1 && len2 <= 1) {
        bdigit_t c;
        neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;
        if (neg1) {
            c = vv1->u.bits.data[0] & (neg2 ? vv2->u.bits.data[0] : ~vv2->u.bits.data[0]);
        } else {
            if (neg2) {
                c = ~vv1->u.bits.data[0] & vv2->u.bits.data[0];
            } else {
                c = vv1->u.bits.data[0] | vv2->u.bits.data[0];
            }
        }
        v = vv->u.bits.val;
        v[0] = c;
        vv->u.bits.data = v;
        vv->u.bits.len = (c != 0);
        vv->u.bits.inv = neg1 | neg2;
        return vv;
    }
    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->u.bits.data; v2 = vv2->u.bits.data;
    neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;

    sz = neg2 ? len2 : len1;
    v = bnew(vv, sz);

    if (neg1) {
        if (neg2) {
            for (i = 0; i < len2; i++) v[i] = v1[i] & v2[i];
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] & ~v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    } else {
        if (neg2) {
            for (i = 0; i < len2; i++) v[i] = ~v1[i] & v2[i];
        } else {
            for (i = 0; i < len2; i++) v[i] = v1[i] | v2[i];
            for (; i < len1; i++) v[i] = v1[i];
        }
    }

    vv->u.bits.inv = neg1 | neg2;
    return normalize(vv, v, sz);
}

static MUST_CHECK value_t xor_(value_t vv1, value_t vv2) {
    size_t blen1, blen2;
    size_t i, len1, len2, sz;
    int neg1, neg2;
    bdigit_t *v1, *v2, *v;
    value_t vv = val_alloc(BITS_OBJ);
    blen1 = vv1->u.bits.bits;
    blen2 = vv2->u.bits.bits;
    vv->u.bits.bits = (blen1 < blen2) ? blen2 : blen1;
    len1 = vv1->u.bits.len;
    len2 = vv2->u.bits.len;

    if (len1 <= 1 && len2 <= 1) {
        bdigit_t c;
        neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;
        c = vv1->u.bits.val[0] ^ vv2->u.bits.val[0];
        v = vv->u.bits.val;
        v[0] = c;
        vv->u.bits.data = v;
        vv->u.bits.len = (c != 0);
        vv->u.bits.inv = neg1 ^ neg2;
        return vv;
    }
    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->u.bits.data; v2 = vv2->u.bits.data;
    neg1 = vv1->u.bits.inv; neg2 = vv2->u.bits.inv;

    sz = len1;
    v = bnew(vv, sz);

    for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
    for (; i < len1; i++) v[i] = v1[i];

    vv->u.bits.inv = neg1 ^ neg2;
    return normalize(vv, v, sz);
}

static MUST_CHECK value_t concat(value_t vv1, value_t vv2) {
    size_t blen;
    size_t sz, bits, i, j, rbits;
    bdigit_t *v1, *v, uv;
    bdigit_t inv;
    value_t vv;

    if (!vv1->u.bits.bits) {
        return val_reference(vv2);
    }
    if (!vv2->u.bits.bits) {
        return val_reference(vv1);
    }
    blen = vv1->u.bits.bits + vv2->u.bits.bits;
    if (blen < vv2->u.bits.bits) err_msg_out_of_memory(); /* overflow */
    sz = blen / SHIFT;
    if (blen % SHIFT) sz++;
    vv = val_alloc(BITS_OBJ);
    v = bnew(vv, sz);
    inv = -(vv2->u.bits.inv ^ vv1->u.bits.inv);

    v1 = vv2->u.bits.data;
    rbits = vv2->u.bits.bits / SHIFT;
    bits = vv2->u.bits.bits & (SHIFT - 1);
    for (i = 0; i < rbits && i < vv2->u.bits.len; i++) v[i] = v1[i] ^ inv;
    for (; i < rbits; i++) v[i] = inv;
    if (i < vv2->u.bits.len) uv = v1[i] ^ inv; else uv = inv;
    if (inv) uv &= (1 << bits) - 1;

    rbits = vv1->u.bits.bits / SHIFT;
    v1 = vv1->u.bits.data;

    if (bits) {
        for (j = 0; j < vv1->u.bits.len; j++) {
            v[i++] = uv | (v1[j] << bits);
            uv = v1[j] >> (SHIFT - bits);
        }
        if (j < rbits) { v[i++] = uv; uv = 0; j++;}
        for (; j < rbits; j++) v[i++] = 0;
        if (i < sz) v[i] = uv;
    } else {
        for (j = 0; j < vv1->u.bits.len; j++) v[i++] = v1[j];
        for (; j < rbits; j++) v[i++] = 0;
    }

    vv->u.bits.inv = vv1->u.bits.inv;
    vv->u.bits.bits = blen;
    return normalize(vv, v, sz);
}

static MUST_CHECK value_t lshift(const value_t vv1, size_t s) {
    size_t i, sz, bits, len1, word;
    int bit;
    bdigit_t *v1, *v, *o;
    value_t vv = val_alloc(BITS_OBJ);

    word = s / SHIFT;
    bit = s % SHIFT;
    v1 = vv1->u.bits.data;
    len1 = vv1->u.bits.len;
    bits = vv1->u.bits.bits + s;
    if (bits < s) err_msg_out_of_memory(); /* overflow */
    sz = word + (bit > 0);
    sz += len1;
    if (sz < len1) err_msg_out_of_memory(); /* overflow */
    v = bnew(vv, sz);
    o = &v[word];
    if (bit) {
        o[len1] = 0;
        for (i = len1; i--;) {
            o[i + 1] |= v1[i] >> (SHIFT - bit);
            o[i] = v1[i] << bit;
        }
    } else if (len1) memmove(o, v1, len1 * sizeof(bdigit_t));
    memset(v, 0, word * sizeof(bdigit_t));

    vv->u.bits.inv = vv1->u.bits.inv;
    vv->u.bits.bits = bits;
    return normalize(vv, v, sz);
}

static MUST_CHECK value_t rshift(const value_t vv1, uval_t s) {
    size_t i, sz, bits, word;
    int bit;
    bdigit_t *v1, *v;
    value_t vv;

    word = s / SHIFT;
    bit = s - word * SHIFT;
    bits = vv1->u.bits.bits;
    if (vv1->u.bits.len <= word || bits <= s) {
        return val_reference(null_bits);
    }
    sz = vv1->u.bits.len - word;
    bits -= s;
    vv = val_alloc(BITS_OBJ);
    v = bnew(vv, sz);
    v1 = vv1->u.bits.data + word;
    if (bit) {
        for (i = 0; i < sz - 1; i++) {
            v[i] = v1[i] >> bit;
            v[i] |= v1[i + 1] << (SHIFT - bit);
        }
        v[i] = v1[i] >> bit;
    } else if (sz) memmove(v, v1, sz * sizeof(bdigit_t));

    vv->u.bits.inv = vv1->u.bits.inv;
    vv->u.bits.bits = bits;
    return normalize(vv, v, sz);
}

static inline MUST_CHECK value_t repeat(oper_t op) {
    value_t vv1 = op->v1, vv;
    bdigit_t *v, *v1;
    bdigit_t uv;
    size_t sz, i, j, rbits, abits, bits;
    size_t blen = vv1->u.bits.bits;
    uval_t rep;

    vv = op->v2->obj->uval(op->v2, &rep, 8*sizeof(uval_t), op->epoint2);
    if (vv) return vv;

    if (!rep || !blen) {
        return val_reference(null_bits);
    }
    if (rep == 1) {
        return val_reference(vv1);
    }

    vv = val_alloc(BITS_OBJ);
    if (blen > SIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
    blen *= rep;
    sz = blen / SHIFT;
    if (blen % SHIFT) sz++;

    v = bnew(vv, sz);
    v1 = vv1->u.bits.data;

    i = 0;
    bits = 0;
    uv = 0;
    rbits = vv1->u.bits.bits / SHIFT;
    abits = vv1->u.bits.bits % SHIFT;
    while (rep--) {
        if (bits) {
            for (j = 0; j < rbits && j < vv1->u.bits.len; j++) {
                v[i++] = uv | (v1[j] << bits);
                uv = (v1[j] >> (SHIFT - bits));
            }
            uv |= v1[j] << bits;
            if (j < rbits) { v[i++] = uv; uv = 0; j++;}
            for (; j < rbits; j++) v[i++] = 0;
            bits += abits;
            if (bits >= SHIFT) {
                v[i++] = uv;
                bits -= SHIFT;
                if (bits) uv = v1[j] >> (abits - bits);
                else uv = 0;
            }
        } else {
            for (j = 0; j < rbits && j < vv1->u.bits.len; j++) v[i++] = v1[j];
            for (; j < rbits; j++) v[i++] = 0;
            uv = v1[j];
            bits = abits;
        }
    }
    if (i < sz) v[i] = uv;

    vv->u.bits.inv = vv1->u.bits.inv;
    vv->u.bits.bits = blen;
    return normalize(vv, v, sz);
}

static inline MUST_CHECK value_t slice(value_t vv2, oper_t op, size_t ln) {
    size_t bo, wo, bl, wl, sz, wl2;
    bdigit_t uv;
    bdigit_t *v, *v1;
    bdigit_t inv;
    int bits;
    value_t vv, vv1 = op->v1;
    size_t length;
    ival_t offs, end, step;

    vv = sliceparams(vv2, ln, &length, &offs, &end, &step, op->epoint2);
    if (vv) return vv;

    if (!length) {
        return val_reference(null_bits);
    }
    inv = -vv1->u.bits.inv;
    if (step == 1) {
        if (length == vv1->u.bits.bits) {
            return val_reference(vv1); /* original bits */
        }

        bo = offs % SHIFT;
        wo = offs / SHIFT;
        bl = length % SHIFT;
        wl = length / SHIFT;

        sz = wl + (bl > 0);
        vv = val_alloc(BITS_OBJ);
        v = bnew(vv, sz);

        v1 = vv1->u.bits.data + wo;
        wl2 = (wo > vv1->u.bits.len) ? 0 : (vv1->u.bits.len - wo);
        if (bo) {
            for (sz = 0; sz < wl; sz++) {
                v[sz] = inv ^ (sz < wl2 ? (v1[sz] >> bo) : 0) ^ ((sz + 1 < wl2) ? (v1[sz + 1] << (SHIFT - bo)) : 0);
            }
            if (bl) {
                v[sz] = sz < wl2 ? (v1[sz] >> bo) : 0;
                if (bl > (SHIFT - bo)) v[sz] |= v1[sz + 1] << (SHIFT - bo);
                v[sz] ^= inv;
            }
        } else {
            for (sz = 0; sz < wl2 && sz < wl; sz++) v[sz] = v1[sz] ^ inv;
            for (; sz < wl; sz++) v[sz] = inv;
            if (bl) v[sz] = inv ^ ((sz < wl2) ? v1[sz] : 0);
        }
        if (bl) v[sz++] &= ((1 << bl) - 1);
    } else {
        sz = length / SHIFT;
        if (length % SHIFT) sz++;
        vv = val_alloc(BITS_OBJ);
        v = bnew(vv, sz);

        uv = inv;
        sz = bits = 0;
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            wo = offs / SHIFT;
            if (wo < vv1->u.bits.len && ((vv1->u.bits.data[wo] >> (offs & (SHIFT - 1))) & 1)) {
                uv ^= 1 << bits;
            }
            bits++;
            if (bits == SHIFT) {
                v[sz++] = uv;
                uv = inv;
                bits = 0;
            }
            offs += step;
        }
        if (bits) v[sz++] = uv & ((1 << bits) - 1);
    }

    vv->u.bits.inv = 0;
    vv->u.bits.bits = length;
    return normalize(vv, v, sz);
}

static inline MUST_CHECK value_t iindex(oper_t op) {
    size_t offs, ln, sz;
    size_t i, o;
    value_t vv1 = op->v1, vv2 = op->v2, vv;
    bdigit_t *v;
    bdigit_t uv;
    bdigit_t inv = -vv1->u.bits.inv;
    int bits;
    value_t err;

    if (vv2->u.funcargs.len != 1) {
        err_msg_argnum(vv2->u.funcargs.len, 1, 1, op->epoint2);
        return val_reference(none_value);
    }
    vv2 = vv2->u.funcargs.val->val;

    ln = vv1->u.bits.bits;

    if (vv2->obj == LIST_OBJ) {
        if (!vv2->u.list.len) {
            return val_reference(null_bits);
        }
        sz = (vv2->u.list.len + SHIFT - 1) / SHIFT;

        vv = val_alloc(BITS_OBJ);
        v = bnew(vv, sz);

        uv = inv;
        bits = sz = 0;
        for (i = 0; i < vv2->u.list.len; i++) {
            err = indexoffs(vv2->u.list.data[i], ln, &offs, op->epoint2);
            if (err) {
                vv->u.bits.data = v;
                val_destroy(vv);
                return err;
            }
            o = offs / SHIFT;
            if (o < vv1->u.bits.len && ((vv1->u.bits.data[o] >> (offs & (SHIFT - 1))) & 1)) {
                uv ^= 1 << bits;
            }
            bits++;
            if (bits == SHIFT) {
                v[sz++] = uv;
                uv = inv;
                bits = 0;
            }
        }
        if (bits) v[sz++] = uv & ((1 << bits) - 1);

        vv->u.bits.inv = 0;
        vv->u.bits.bits = vv2->u.list.len;
        return normalize(vv, v, sz);
    }
    if (vv2->obj == COLONLIST_OBJ) {
        return slice(vv2, op, ln);
    }
    err = indexoffs(vv2, ln, &offs, op->epoint2);
    if (err) return err;

    uv = inv;
    o = offs / SHIFT;
    if (o < vv1->u.bits.len && ((vv1->u.bits.data[o] >> (offs & (SHIFT - 1))) & 1)) {
        uv ^= 1;
    }
    return bits_from_bool(uv & 1);
}

static MUST_CHECK value_t calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    value_t tmp, result, err;
    ival_t shift;

    if (op->op == &o_INDEX) {
        return iindex(op);
    }
    if (op->op == &o_X) {
        return repeat(op);
    }
    switch (v2->obj->type) {
    case T_BOOL:
        tmp = bits_from_bool(op->v2->u.boolean);
        op->v2 = tmp;
        result = calc2(op);
        val_destroy(tmp);
        op->v2 = v2;
        return result;
    case T_BITS:
        switch (op->op->u.oper.op) {
        case O_AND: return and_(v1, v2);
        case O_OR: return or_(v1, v2);
        case O_XOR: return xor_(v1, v2);
        case O_CONCAT: return concat(v1, v2);
        default: break;
        }
        /* fall through */
    case T_INT:
        switch (op->op->u.oper.op) {
        case O_LSHIFT:
            err = v2->obj->ival(v2, &shift, 8*sizeof(ival_t), op->epoint2);
            if (err) return err;
            return (shift < 0) ? rshift(v1, -shift) : lshift(v1, shift);
        case O_RSHIFT:
            err = v2->obj->ival(v2, &shift, 8*sizeof(ival_t), op->epoint2);
            if (err) return err;
            return (shift < 0) ? lshift(v1, -shift) : rshift(v1, shift);
        default: break;
        }
        tmp = int_from_bits(v1);
        op->v1 = tmp;
        result = tmp->obj->calc2(op);
        val_destroy(tmp);
        op->v1 = v1;
        return result;
    default:
        if (op->op != &o_MEMBER) {
            return op->v2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    value_t tmp, result;
    switch (v1->obj->type) {
    case T_BOOL:
        tmp = bits_from_bool(op->v1->u.boolean);
        op->v1 = tmp;
        result = tmp->obj->calc2(op);
        val_destroy(tmp);
        op->v1 = v1;
        return result;
    case T_BITS:
        switch (op->op->u.oper.op) {
        case O_AND: return and_(v1, v2);
        case O_OR: return or_(v1, v2);
        case O_XOR: return xor_(v1, v2);
        case O_CONCAT: return concat(v1, v2);
        default: break;
        }
        /* fall through */
    case T_INT:
        tmp = int_from_bits(v2);
        op->v2 = tmp;
        result = v1->obj->calc2(op);
        val_destroy(tmp);
        op->v2 = v2;
        return result;
    default: break;
    }
    return obj_oper_error(op);
}

void bitsobj_init(void) {
    obj_init(&obj, T_BITS, "<bits>");
    obj.destroy = destroy;
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
    obj.len = len;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
