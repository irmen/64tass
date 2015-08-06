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
#include "bitsobj.h"
#include "eval.h"
#include "variables.h"
#include "unicode.h"
#include "encoding.h"
#include "boolobj.h"
#include "floatobj.h"
#include "codeobj.h"
#include "error.h"
#include "strobj.h"
#include "bytesobj.h"
#include "intobj.h"
#include "error.h"
#include "listobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"

#define SHIFT (8 * sizeof(bdigit_t))

static Type obj;

Type *BITS_OBJ = &obj;
Bits *null_bits;
Bits *inv_bits;
Bits *bits_value[2];

static MUST_CHECK Obj *bits_from_int(const Int *);

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    Obj *err, *ret;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_BITS: return val_reference(v1);
    case T_BOOL: return (Obj *)ref_bits(bits_value[((Bool *)v1)->boolean]);
    case T_STR: return bits_from_str((Str *)v1, epoint);
    case T_BYTES: return (Obj *)bits_from_bytes((Bytes *)v1);
    case T_INT: return bits_from_int((Int *)v1);
    case T_CODE: return bits_from_code((Code *)v1, epoint);
    case T_FLOAT: 
         err = (Obj *)int_from_float((Float *)v1);
         if (err->obj != INT_OBJ) return err;
         ret = bits_from_int((Int *)err);
         val_destroy(err);
         return ret;
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return (Obj *)ref_none();
}

static inline size_t bitslen(const Bits *v1) {
    ssize_t len = v1->len;
    return (len < 0) ? ~len : len;
}

static void destroy(Obj *o1) {
    Bits *v1 = (Bits *)o1;
    if (v1->val != v1->data) free(v1->data);
}

static MUST_CHECK Bits *new_bits(size_t len) {
    Bits *v = (Bits *)val_alloc(BITS_OBJ);
    if (len > sizeof(v->val)/sizeof(v->val[0])) {
        if (len > SIZE_MAX / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
        v->data = (bdigit_t *)mallocx(len * sizeof(bdigit_t));
    } else {
        v->data = v->val;
    }
    return v;
}

static MUST_CHECK Obj *invert(const Bits *v1) {
    size_t sz = bitslen(v1);
    Bits *v = new_bits(sz);
    v->bits = v1->bits;
    v->len = ~v1->len;
    if (sz) {
        memcpy(v->data, v1->data, sz * sizeof(bdigit_t));
    } else {
        v->data[0] = 0;
    }
    return &v->v;
}

static MUST_CHECK Obj *normalize(Bits *v, size_t sz, int neg) {
    bdigit_t *d = v->data;
    while (sz && !d[sz - 1]) sz--;
    if (v->val != d && sz <= sizeof(v->val)/sizeof(v->val[0])) {
        memcpy(v->val, d, sz * sizeof(bdigit_t));
        free(d);
        v->data = v->val;
    }
    /*if (sz >= SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    v->len = neg ? ~sz : sz;
    return &v->v;
}

static int same(const Obj *o1, const Obj *o2) {
    const Bits *v1 = (const Bits *)o1, *v2 = (const Bits *)o2;
    if (o2->obj != BITS_OBJ || v1->len != v2->len || v1->bits != v2->bits) return 0;
    return !memcmp(v1->data, v2->data, bitslen(v1) * sizeof(bdigit_t));
}

static MUST_CHECK Obj *truth(Obj *o1, enum truth_e type, linepos_t UNUSED(epoint)) {
    const Bits *v1 = (const Bits *)o1;
    size_t i, sz, sz2;
    bdigit_t b, inv;
    switch (type) {
    case TRUTH_ALL:
        sz = bitslen(v1);
        sz2 = v1->bits / SHIFT;
        if (sz2 > sz) sz2 = sz;
        inv = -(v1->len >= 0);
        for (i = 0; i < sz2; i++) {
            if (v1->data[i] != inv) return (Obj *)ref_bool(false_value);
        }
        b = (i >= sz) ? 0 : v1->data[i];
        b ^= inv;
        b <<= SHIFT - v1->bits % SHIFT;
        return truth_reference(!b);
    case TRUTH_ANY:
        return truth_reference(v1->len != 0 && v1->len != ~0);
    default:
        return truth_reference(!!v1->len);
    }
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint)) {
    const Bits *v1 = (const Bits *)o1;
    size_t len, i, len2, sz;
    uint8_t *s;
    int inv;
    Str *v = new_str();

    len2 = v1->bits;
    sz = bitslen(v1);
    inv = (v1->len < 0);
    if (len2 & 3) {
        len = 1 + inv;
        len += len2;
        s = str_create_elements(v, len);

        len = 0;
        if (inv) s[len++] = '~';
        s[len++] = '%';
        for (i = len2; i--;) {
            size_t j = i / SHIFT;
            s[len++] = (j >= sz) ? 0x30 : 0x30 | ((v1->data[j] >> (i & (SHIFT - 1))) & 1);
        }
    } else {
        static const char *hex = "0123456789abcdef";
        len2 /= 4;
        len = 1 + inv;
        len += len2;
        s = str_create_elements(v, len);

        len = 0;
        if (inv) s[len++] = '~';
        s[len++] = '$';
        for (i = len2; i--;) {
            size_t j = i / (2 * sizeof(bdigit_t));
            s[len++] = (j >= sz) ? 0x30 : (uint8_t)hex[(v1->data[j] >> ((i & (2 * sizeof(bdigit_t) - 1)) * 4)) & 15];
        }
    }
    v->len = len;
    v->chars = len;
    v->data = s;
    return &v->v;
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    const Bits *v1 = (const Bits *)o1;
    size_t l;
    unsigned int h;

    switch (v1->len) {
    case ~1: *hs = (~v1->data[0]) & ((~(unsigned int)0) >> 1); return NULL;
    case ~0: *hs = ((~(unsigned int)0) >> 1); return NULL;
    case 0: *hs = 0; return NULL;
    case 1: *hs = v1->data[0] & ((~(unsigned int)0) >> 1); return NULL;
    default: break;
    }
    h = -(v1->len < 0);
    if (h) {
        l = ~v1->len;
        while (l--) {
            h -= v1->val[l];
        }
    } else {
        l = v1->len;
        while (l--) {
            h += v1->val[l];
        }
    }
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, int bits, linepos_t epoint) {
    Bits *v1 = (Bits *)o1;
    Error *v;
    switch (v1->len) {
    case ~1: *iv = v1->data[0];
            if (bits < (int)SHIFT && (uval_t)*iv >> bits) break;
            *iv = ~*iv;
            return NULL;
    case ~0: *iv = ~(ival_t)0; return NULL;
    case 0: *iv = 0; return NULL;
    case 1: *iv = v1->data[0];
            if (bits < (int)SHIFT && (uval_t)*iv >> bits) break;
            return NULL;
    default: break;
    }
    v = new_error(ERROR_____CANT_IVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = (Obj *)ref_bits(v1);
    return v;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, int bits, linepos_t epoint) {
    Bits *v1 = (Bits *)o1;
    Error *v;
    switch (v1->len) {
    case ~1: *uv = v1->data[0];
            if (bits < (int)SHIFT && *uv >> bits) break;
            *uv = ~*uv;
            return NULL;
    case ~0: *uv = ~(uval_t)0; return NULL;
    case 0: *uv = 0; return NULL;
    case 1: *uv = v1->data[0];
            if (bits < (int)SHIFT && *uv >> bits) break;
            return NULL;
    default: break;
    }
    v = new_error(ERROR_____CANT_UVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = (Obj *)ref_bits(v1);
    return v;
}

MUST_CHECK Obj *float_from_bits(const Bits *v1, linepos_t epoint) {
    double d;
    size_t i, len1;
    switch (v1->len) {
    case ~1: d = -1.0 - v1->data[0]; break;
    case ~0: d = -1.0; break;
    case 0: d = 0.0; break;
    case 1: d = v1->data[0]; break;
    default:
        d = -(v1->len < 0);
        len1 = d ? ~v1->len : v1->len;
        for (i = 0; i < len1; i++) {
            if (v1->len < 0) d -= ldexp(v1->data[i], i * SHIFT);
            else d += ldexp(v1->data[i], i * SHIFT);
        }
        return (Obj *)float_from_double(d, epoint);
    }
    return (Obj *)new_float(d);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Bits *v1 = (Bits *)o1;
    ssize_t len = v1->len;
    if (len < 0) return (Obj *)ref_int(minus1_value);
    return (Obj *)ref_int(int_value[len > 0]);
}

static MUST_CHECK Obj *absolute(Obj *o1, linepos_t epoint) {
    Bits *v1 = (Bits *)o1;
    Obj *tmp, *ret;
    tmp = (Obj *)int_from_bits(v1);
    ret = tmp->obj->abs(tmp, epoint);
    val_destroy(tmp);
    return ret;
}

static MUST_CHECK Obj *len(Obj *o1, linepos_t UNUSED(epoint)) {
    Bits *v1 = (Bits *)o1;
    return (Obj *)int_from_size(v1->bits);
}

MUST_CHECK Bits *ibits_from_bool(int i) {
    Bits *v = new_bits(1);
    v->data[0] = i;
    v->len = ~(i != 0);
    v->bits = 1;
    return v;
}

MUST_CHECK Bits *bits_from_bools(int i, int j) {
    Bits *v = new_bits(1);
    v->data[0] = (i << 1) | j;
    v->len = i | j;
    v->bits = 2;
    return v;
}

static MUST_CHECK Bits *bits_from_u24(uint32_t i) {
    Bits *v = new_bits(1);
    v->data[0] = i;
    v->len = (i != 0);
    v->bits = 24;
    return v;
}

MUST_CHECK Bits *bits_from_uval(uval_t i, int bits) {
    Bits *v = new_bits(1);
    v->data[0] = i;
    v->len = (i != 0);
    v->bits = bits;
    return v;
}

MUST_CHECK Bits *bits_from_hexstr(const uint8_t *s, size_t *ln, size_t *ln2) {
    size_t i, j, k, sz;
    int bits;
    bdigit_t *d, uv;
    Bits *v;

    i = k = 0;
    if (s[0] != '_') {
        uv = 0;
        for (;;k++) {
            uint8_t c = s[k], c2 = c ^ 0x30;
            if (c2 < 10) {
                uv = (uv << 4) | c2;
                continue;
            }
            c2 = (c | 0x20) - 0x61;
            if (c2 < 6) {
                uv = (uv << 4) | (c2 + 10);
                continue;
            }
            if (c != '_') break;
            i++;
        }
        while (k && s[k - 1] == '_') k--;
    }
    *ln = k;
    i = k - i;
    *ln2 = i;
    if (i <= 2 * sizeof(bdigit_t)) {
        if (!i) {
            return ref_bits(null_bits);
        }
        v = new_bits(1);
        v->data[0] = uv;
        v->len = (uv != 0);
        v->bits = i * 4;
        return v;
    }

    if (i > SIZE_MAX / 4) err_msg_out_of_memory(); /* overflow */

    sz = i / (2 * sizeof(bdigit_t));
    if (i % (2 * sizeof(bdigit_t))) sz++;
    v = new_bits(sz);
    v->bits = i * 4;
    d = v->data;

    uv = bits = j = 0;
    while (k--) {
        uint8_t c = s[k];
        if (c < 0x40) uv |= (c & 15) << bits;
        else if (c == '_') continue;
        else uv |= ((c & 7) + 9) << bits;
        if (bits == SHIFT - 4) {
            d[j++] = uv;
            bits = uv = 0;
        } else bits += 4;
    }
    if (bits) d[j] = uv;

    return (Bits *)normalize(v, sz, 0);
}

MUST_CHECK Bits *bits_from_binstr(const uint8_t *s, size_t *ln, size_t *ln2) {
    size_t i, j, k, sz;
    int bits;
    bdigit_t *d, uv;
    Bits *v;

    i = k = 0;
    if (s[0] != '_') {
        uv = 0;
        for (;;k++) {
            uint8_t c = s[k];
            if ((c & 0xfe) == 0x30) {
                uv = (uv << 1) | (c & 1);
                continue;
            }
            if (c != '_') break;
            i++;
        }
        while (k && s[k - 1] == '_') k--;
    }
    *ln = k;
    i = k - i;
    *ln2 = i;
    if (i <= 8 * sizeof(bdigit_t)) {
        if (!i) {
            return ref_bits(null_bits);
        }
        v = new_bits(1);
        v->data[0] = uv;
        v->len = (uv != 0);
        v->bits = i;
        return v;
    }

    sz = i / SHIFT;
    if (i % SHIFT) sz++;
    v = new_bits(sz);
    v->bits = i;
    d = v->data;

    uv = bits = j = 0;
    while (k--) {
        uint8_t c = s[k];
        if (c == 0x31) uv |= 1 << bits;
        else if (c == '_') continue;
        if (bits == SHIFT - 1) {
            d[j++] = uv;
            bits = uv = 0;
        } else bits++;
    }
    if (bits) d[j] = uv;

    return (Bits *)normalize(v, sz, 0);
}

MUST_CHECK Obj *bits_from_str(const Str *v1, linepos_t epoint) {
    int ch;
    Bits *v;

    if (actual_encoding) {
        unsigned int bits;
        size_t j, sz, osz;
        bdigit_t *d, uv;

        if (!v1->len) {
            return (Obj *)ref_bits(null_bits);
        }

        if (v1->len <= sizeof(v->val)) sz = sizeof(v->val) / sizeof(v->val[0]);
        else {
            sz = v1->len / sizeof(bdigit_t);
            if (v1->len % sizeof(bdigit_t)) sz++;
        }
        v = new_bits(sz);
        d = v->data;

        uv = bits = j = 0;
        encode_string_init(v1, epoint);
        while ((ch = encode_string()) != EOF) {
            uv |= (uint8_t)ch << bits;
            if (bits == SHIFT - 8) {
                if (j >= sz) {
                    if (v->val == d) {
                        sz = 16 / sizeof(bdigit_t);
                        d = (bdigit_t *)mallocx(sz * sizeof(bdigit_t));
                        memcpy(d, v->val, j * sizeof(bdigit_t));
                    } else {
                        sz += 1024 / sizeof(bdigit_t);
                        if (/*sz < 1024 / sizeof(bdigit_t) ||*/ sz > SIZE_MAX / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
                        d = (bdigit_t *)reallocx(d, sz * sizeof(bdigit_t));
                    }
                }
                d[j++] = uv;
                bits = uv = 0;
            } else bits += 8;
        }
        if (bits) {
            if (j >= sz) {
                sz++;
                if (v->val == d) {
                    d = (bdigit_t *)mallocx(sz * sizeof(bdigit_t));
                    memcpy(d, v->val, j * sizeof(bdigit_t));
                } else {
                    if (/*sz < 1 ||*/ sz > SIZE_MAX / sizeof(bdigit_t)) err_msg_out_of_memory(); /* overflow */
                    d = (bdigit_t *)reallocx(d, sz * sizeof(bdigit_t));
                }
            }
            d[j] = uv;
            osz = j + 1;
        } else osz = j;

        while (osz && !d[osz - 1]) osz--;
        if (v->val != d) {
            if (osz <= sizeof(v->val)/sizeof(v->val[0])) {
                memcpy(v->val, d, osz * sizeof(bdigit_t));
                free(d);
                d = v->val;
            } else if (osz < sz) {
                d = (bdigit_t *)reallocx(d, osz * sizeof(bdigit_t));
            }
        }
        v->data = d;
        v->len = osz;
        v->bits = j * SHIFT + bits;
        if (/*v->bits < bits ||*/ j > SIZE_MAX / SHIFT /*|| osz > SSIZE_MAX*/) err_msg_out_of_memory(); /* overflow */
        return &v->v;
    }
    if (v1->chars == 1) {
        uint32_t ch2 = v1->data[0];
        if (ch2 & 0x80) utf8in(v1->data, &ch2);
        return (Obj *)bits_from_u24(ch2);
    }
    return (Obj *)new_error(ERROR_BIG_STRING_CO, epoint);
}

MUST_CHECK Bits *bits_from_bytes(const Bytes *v1) {
    int bits;
    size_t i, j, sz, len1;
    bdigit_t *d, uv;
    Bits *v;
    int inv = v1->len < 0;

    len1 = inv ? ~v1->len : v1->len;
    if (!len1) {
        return ref_bits(inv ? inv_bits : null_bits);
    }

    if (len1 > SIZE_MAX / 8) err_msg_out_of_memory(); /* overflow */

    sz = len1 / sizeof(bdigit_t);
    if (len1 % sizeof(bdigit_t)) sz++;
    v = new_bits(sz);
    v->bits = len1 * 8;
    d = v->data;

    uv = bits = j = i = 0;
    while (len1 > i) {
        uv |= v1->data[i++] << bits;
        if (bits == SHIFT - 8) {
            d[j++] = uv;
            bits = uv = 0;
        } else bits += 8;
    }
    if (bits) d[j] = uv;

    return (Bits *)normalize(v, sz, inv);
}

static MUST_CHECK Obj *bits_from_int(const Int *v1) {
    unsigned int inv;
    size_t i, sz, bits;
    bdigit_t *d, d2;
    const digit_t *b;
    Bits *v;

    if (!v1->len) return (Obj *)ref_bits(null_bits);
    if (v1->len == -1 && v1->data[0] == 1) return (Obj *)ref_bits(inv_bits);

    inv = v1->len < 0;
    sz = inv ? -v1->len : v1->len;
    v = new_bits(sz);
    d = v->data;

    b = v1->data;
    if (inv) {
        int c = 0;
        for (i = 0; !c && i < sz; i++) {
            d[i] = (c = b[i]) - 1;
        }
        for (; i < sz; i++) {
            d[i] = b[i];
        }
        d[i] = c;
    } else memcpy(d, b, sz * sizeof(bdigit_t));

    d2 = d[sz - 1];
    for (bits = 0; d2; bits++) d2 >>= 1;

    v->bits = (sz - 1) * SHIFT + bits;

    return normalize(v, sz, inv);
}

static ssize_t icmp(const Bits *vv1, const Bits *vv2) {
    ssize_t i;
    size_t j;
    bdigit_t a, b;
    i = vv1->len - vv2->len;
    if (i) return i;
    j = bitslen(vv1);
    while (j--) {
        a = vv1->data[j]; b = vv2->data[j];
        if (a > b) return (vv1->len < 0) ? -1: 1;
        if (a < b) return (vv1->len < 0) ? 1: -1;
    }
    return 0;
}

static bdigit_t ldigit(const Bits *v1) {
    ssize_t ln = v1->len;
    if (ln > 0) return v1->data[0];
    if (ln < ~(ssize_t)0) return ~v1->data[0];
    return ln;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Bits *v1 = (Bits *)op->v1;
    Obj *tmp, *v;
    bdigit_t uv;
    switch (op->op->op) {
    case O_BANK: return (Obj *)bytes_from_u8(ldigit(v1) >> 16);
    case O_HIGHER: return (Obj *)bytes_from_u8(ldigit(v1) >> 8);
    case O_LOWER: return (Obj *)bytes_from_u8(ldigit(v1));
    case O_HWORD: return (Obj *)bytes_from_u16(ldigit(v1) >> 8);
    case O_WORD: return (Obj *)bytes_from_u16(ldigit(v1));
    case O_BSWORD:
        uv = ldigit(v1);
        return (Obj *)bytes_from_u16((uint8_t)(uv >> 8) | (uint16_t)(uv << 8));
    case O_INV: return invert(v1);
    case O_NEG:
    case O_POS:
    case O_STRING:
        tmp = (Obj *)int_from_bits(v1);
        op->v1 = tmp;
        v = tmp->obj->calc1(op);
        val_destroy(tmp);
        op->v1 = &v1->v;
        return v;
    default:
        break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *and_(const Bits *vv1, const Bits *vv2) {
    size_t blen1, blen2, blen;
    size_t i, len1, len2, sz;
    int neg1, neg2;
    bdigit_t *v1, *v2, *v;
    Bits *vv;
    blen1 = vv1->bits;
    blen2 = vv2->bits;
    if (blen1 < blen2) {
        blen = (vv1->len < 0) ? blen2 : blen1;
    } else {
        blen = (vv2->len < 0) ? blen1 : blen2;
    }
    len1 = bitslen(vv1);
    len2 = bitslen(vv2);

    if (len1 <= 1 && len2 <= 1) {
        bdigit_t c;
        neg1 = vv1->len < 0; neg2 = vv2->len < 0;
        if (neg1) {
            if (neg2) {
                c = vv1->data[0] | vv2->data[0];
            } else {
                c = ~vv1->data[0] & vv2->data[0];
            }
        } else {
            c = vv1->data[0] & (neg2 ? ~vv2->data[0] : vv2->data[0]);
        }
        vv = (Bits *)val_alloc(BITS_OBJ);
        vv->data = v = vv->val;
        v[0] = c;
        vv->len = (neg1 & neg2) ? ~(c != 0) : (c != 0);
        vv->bits = blen;
        return &vv->v;
    }
    if (len1 < len2) {
        const Bits *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->data; v2 = vv2->data;
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = neg2 ? len1 : len2;
    vv = new_bits(sz);
    vv->bits = blen;
    v = vv->data;

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

    return normalize(vv, sz, neg1 & neg2);
}

static MUST_CHECK Obj *or_(const Bits *vv1, const Bits *vv2) {
    size_t blen1, blen2, blen;
    size_t i, len1, len2, sz;
    int neg1, neg2;
    bdigit_t *v1, *v2, *v;
    Bits *vv;
    blen1 = vv1->bits;
    blen2 = vv2->bits;
    if (blen1 < blen2) {
        blen = (vv1->len < 0) ? blen1 : blen2;
    } else {
        blen = (vv2->len < 0) ? blen2 : blen1;
    }
    len1 = bitslen(vv1);
    len2 = bitslen(vv2);

    if (len1 <= 1 && len2 <= 1) {
        bdigit_t c;
        neg1 = vv1->len < 0; neg2 = vv2->len < 0;
        if (neg1) {
            c = vv1->data[0] & (neg2 ? vv2->data[0] : ~vv2->data[0]);
        } else {
            if (neg2) {
                c = ~vv1->data[0] & vv2->data[0];
            } else {
                c = vv1->data[0] | vv2->data[0];
            }
        }
        vv = (Bits *)val_alloc(BITS_OBJ);
        vv->data = v = vv->val;
        v[0] = c;
        vv->len = (neg1 | neg2) ? ~(c != 0) : (c != 0);
        vv->bits = blen;
        return &vv->v;
    }
    if (len1 < len2) {
        const Bits *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->data; v2 = vv2->data;
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = neg2 ? len2 : len1;
    vv = new_bits(sz);
    vv->bits = blen;
    v = vv->data;

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

    return normalize(vv, sz, neg1 | neg2);
}

static MUST_CHECK Obj *xor_(const Bits *vv1, const Bits *vv2) {
    size_t blen1, blen2, blen;
    size_t i, len1, len2, sz;
    int neg1, neg2;
    bdigit_t *v1, *v2, *v;
    Bits *vv;
    blen1 = vv1->bits;
    blen2 = vv2->bits;
    blen = (blen1 < blen2) ? blen2 : blen1;
    len1 = bitslen(vv1);
    len2 = bitslen(vv2);

    if (len1 <= 1 && len2 <= 1) {
        bdigit_t c;
        neg1 = vv1->len < 0; neg2 = vv2->len < 0;
        c = vv1->val[0] ^ vv2->val[0];
        vv = (Bits *)val_alloc(BITS_OBJ);
        vv->data = v = vv->val;
        v[0] = c;
        vv->len = (neg1 ^ neg2) ? ~(c != 0) : (c != 0);
        vv->bits = blen;
        return &vv->v;
    }
    if (len1 < len2) {
        const Bits *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    v1 = vv1->data; v2 = vv2->data;
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = len1;
    vv = new_bits(sz);
    vv->bits = blen;
    v = vv->data;

    for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
    for (; i < len1; i++) v[i] = v1[i];

    return normalize(vv, sz, neg1 ^ neg2);
}

static MUST_CHECK Obj *concat(Bits *vv1, Bits *vv2) {
    size_t blen;
    size_t sz, bits, i, j, rbits, l;
    bdigit_t *v1, *v, uv;
    bdigit_t inv;
    Bits *vv;

    if (!vv1->bits) {
        return (Obj *)ref_bits(vv2);
    }
    if (!vv2->bits) {
        return (Obj *)ref_bits(vv1);
    }
    blen = vv1->bits + vv2->bits;
    if (blen < vv2->bits) err_msg_out_of_memory(); /* overflow */
    sz = blen / SHIFT;
    if (blen % SHIFT) sz++;
    vv = new_bits(sz);
    v = vv->data;
    inv = -((vv2->len ^ vv1->len) < 0);

    v1 = vv2->data;
    rbits = vv2->bits / SHIFT;
    bits = vv2->bits & (SHIFT - 1);
    l = bitslen(vv2);
    for (i = 0; i < rbits && i < l; i++) v[i] = v1[i] ^ inv;
    for (; i < rbits; i++) v[i] = inv;
    if (i < l) uv = v1[i] ^ inv; else uv = inv;
    if (inv) uv &= (1 << bits) - 1;

    rbits = vv1->bits / SHIFT;
    v1 = vv1->data;

    l = bitslen(vv1);
    if (bits) {
        for (j = 0; j < l; j++) {
            v[i++] = uv | (v1[j] << bits);
            uv = v1[j] >> (SHIFT - bits);
        }
        if (j < rbits) { v[i++] = uv; uv = 0; j++;}
        for (; j < rbits; j++) v[i++] = 0;
        if (i < sz) v[i] = uv;
    } else {
        for (j = 0; j < l; j++) v[i++] = v1[j];
        for (; j < rbits; j++) v[i++] = 0;
    }

    vv->bits = blen;
    return normalize(vv, sz, vv1->len < 0);
}

static MUST_CHECK Obj *lshift(const Bits *vv1, size_t s) {
    size_t i, sz, bits, len1, word;
    int bit;
    bdigit_t *v1, *v, *o;
    Bits *vv;

    word = s / SHIFT;
    bit = s % SHIFT;
    v1 = vv1->data;
    len1 = bitslen(vv1);
    bits = vv1->bits + s;
    if (bits < s) err_msg_out_of_memory(); /* overflow */
    sz = word + (bit > 0);
    sz += len1;
    if (sz < len1) err_msg_out_of_memory(); /* overflow */
    vv = new_bits(sz);
    v = vv->data;
    o = &v[word];
    if (bit) {
        o[len1] = 0;
        for (i = len1; i--;) {
            o[i + 1] |= v1[i] >> (SHIFT - bit);
            o[i] = v1[i] << bit;
        }
    } else if (len1) memmove(o, v1, len1 * sizeof(bdigit_t));
    memset(v, 0, word * sizeof(bdigit_t));

    vv->bits = bits;
    return normalize(vv, sz, vv1->len < 0);
}

static MUST_CHECK Obj *rshift(const Bits *vv1, uval_t s) {
    size_t i, sz, bits, word, l;
    int bit;
    bdigit_t *v1, *v;
    Bits *vv;

    word = s / SHIFT;
    bit = s - word * SHIFT;
    bits = vv1->bits;
    l = bitslen(vv1);
    if (l <= word || bits <= s) {
        return (Obj *)ref_bits((vv1->len < 0) ? inv_bits : null_bits);
    }
    sz = l - word;
    bits -= s;
    vv = new_bits(sz);
    v = vv->data;
    v1 = vv1->data + word;
    if (bit) {
        for (i = 0; i < sz - 1; i++) {
            v[i] = v1[i] >> bit;
            v[i] |= v1[i + 1] << (SHIFT - bit);
        }
        v[i] = v1[i] >> bit;
    } else if (sz) memmove(v, v1, sz * sizeof(bdigit_t));

    vv->bits = bits;
    return normalize(vv, sz, vv1->len < 0);
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Bits *vv1 = (Bits *)op->v1, *vv;
    bdigit_t *v, *v1;
    bdigit_t uv;
    size_t sz, i, j, rbits, abits, bits, l;
    size_t blen = vv1->bits;
    uval_t rep;
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8*sizeof(uval_t), op->epoint2);
    if (err) return &err->v;

    if (!rep || !blen) {
        return (Obj *)ref_bits((vv1->len < 0) ? inv_bits : null_bits);
    }
    if (rep == 1) {
        return (Obj *)ref_bits(vv1);
    }

    if (blen > SIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
    blen *= rep;
    sz = blen / SHIFT;
    if (blen % SHIFT) sz++;

    vv = new_bits(sz);
    v = vv->data;
    v1 = vv1->data;

    i = 0;
    bits = 0;
    uv = 0;
    rbits = vv1->bits / SHIFT;
    abits = vv1->bits % SHIFT;
    l = bitslen(vv1);
    while (rep--) {
        if (bits) {
            for (j = 0; j < rbits && j < l; j++) {
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
            for (j = 0; j < rbits && j < l; j++) v[i++] = v1[j];
            for (; j < rbits; j++) v[i++] = 0;
            uv = v1[j];
            bits = abits;
        }
    }
    if (i < sz) v[i] = uv;

    vv->bits = blen;
    return normalize(vv, sz, vv1->len < 0);
}

static inline MUST_CHECK Obj *slice(Colonlist *vv2, oper_t op, size_t ln) {
    size_t bo, wo, bl, wl, sz, wl2, l;
    bdigit_t uv;
    bdigit_t *v, *v1;
    bdigit_t inv;
    int bits;
    Bits *vv, *vv1 = (Bits *)op->v1;
    size_t length;
    ival_t offs, end, step;
    Obj *err;

    err = sliceparams(vv2, ln, &length, &offs, &end, &step, op->epoint2);
    if (err) return err;

    if (!length) {
        return (Obj *)ref_bits(null_bits);
    }
    inv = -(vv1->len < 0);
    if (step == 1) {
        if (length == vv1->bits && !inv) {
            return (Obj *)ref_bits(vv1); /* original bits */
        }

        bo = offs % SHIFT;
        wo = offs / SHIFT;
        bl = length % SHIFT;
        wl = length / SHIFT;

        sz = wl + (bl > 0);
        vv = new_bits(sz);
        v = vv->data;

        l = bitslen(vv1);
        v1 = vv1->data + wo;
        wl2 = (wo > l) ? 0 : (l - wo);
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
        vv = new_bits(sz);
        v = vv->data;

        uv = inv;
        sz = bits = 0;
        l = bitslen(vv1);
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            wo = offs / SHIFT;
            if (wo < l && ((vv1->data[wo] >> (offs & (SHIFT - 1))) & 1)) {
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

    vv->bits = length;
    return normalize(vv, sz, 0);
}

static inline MUST_CHECK Obj *iindex(oper_t op) {
    size_t offs, ln, sz;
    size_t i, o;
    Bits *vv1 = (Bits *)op->v1, *vv;
    Obj *vv2 = op->v2;
    bdigit_t *v;
    bdigit_t uv;
    bdigit_t inv = -(vv1->len < 0);
    int bits;
    Error *err;
    Funcargs *args = (Funcargs *)vv2;

    if (args->len != 1) {
        err_msg_argnum(args->len, 1, 1, op->epoint2);
        return (Obj *)ref_none();
    }
    vv2 = args->val->val;

    ln = vv1->bits;

    if (vv2->obj == LIST_OBJ) {
        List *list = (List *)vv2;
        if (!list->len) {
            return (Obj *)ref_bits(null_bits);
        }
        sz = (list->len + SHIFT - 1) / SHIFT;

        vv = new_bits(sz);
        v = vv->data;

        uv = inv;
        bits = sz = 0;
        for (i = 0; i < list->len; i++) {
            err = indexoffs(list->data[i], ln, &offs, op->epoint2);
            if (err) {
                val_destroy(&vv->v);
                return &err->v;
            }
            o = offs / SHIFT;
            if (o < bitslen(vv1) && ((vv1->data[o] >> (offs & (SHIFT - 1))) & 1)) {
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

        vv->bits = list->len;
        return normalize(vv, sz, 0);
    }
    if (vv2->obj == COLONLIST_OBJ) {
        return slice((Colonlist *)vv2, op, ln);
    }
    err = indexoffs(vv2, ln, &offs, op->epoint2);
    if (err) return &err->v;

    uv = inv;
    o = offs / SHIFT;
    if (o < bitslen(vv1) && ((vv1->data[o] >> (offs & (SHIFT - 1))) & 1)) {
        uv ^= 1;
    }
    return (Obj *)ref_bits(bits_value[uv & 1]);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Bits *v1 = (Bits *)op->v1;
    Obj *o2 = op->v2;
    Obj *tmp, *result;
    Error *err;
    ival_t shift;
    ssize_t val;

    if (op->op == &o_INDEX) {
        return (Obj *)iindex(op);
    }
    if (op->op == &o_X) {
        return repeat(op);
    }
    switch (o2->obj->type) {
    case T_BOOL:
        {
            Bool *v2 = (Bool *)o2;
            tmp = (Obj *)ref_bits(bits_value[v2->boolean]);
            op->v2 = tmp;
            result = calc2(op);
            val_destroy(tmp);
            op->v2 = &v2->v;
            return result;
        }
    case T_BITS:
        {
            Bits *v2 = (Bits *)o2;
            switch (op->op->op) {
            case O_CMP:
                val = icmp(v1, v2);
                if (val < 0) return (Obj *)ref_int(minus1_value);
                return (Obj *)ref_int(int_value[val > 0]);
            case O_EQ: return truth_reference(icmp(v1, v2) == 0);
            case O_NE: return truth_reference(icmp(v1, v2) != 0);
            case O_LT: return truth_reference(icmp(v1, v2) < 0);
            case O_LE: return truth_reference(icmp(v1, v2) <= 0);
            case O_GT: return truth_reference(icmp(v1, v2) > 0);
            case O_GE: return truth_reference(icmp(v1, v2) >= 0);
            case O_AND: return and_(v1, v2);
            case O_OR: return or_(v1, v2);
            case O_XOR: return xor_(v1, v2);
            case O_CONCAT: return concat(v1, v2);
            case O_IN: return obj_oper_error(op); /* TODO */
            default: break;
            }
            /* fall through */
        }
    case T_INT:
        switch (op->op->op) {
        case O_LSHIFT:
            err = o2->obj->ival(o2, &shift, 8*sizeof(ival_t), op->epoint2);
            if (err) return &err->v;
            return (shift < 0) ? rshift(v1, -shift) : lshift(v1, shift);
        case O_RSHIFT:
            err = o2->obj->ival(o2, &shift, 8*sizeof(ival_t), op->epoint2);
            if (err) return &err->v;
            return (shift < 0) ? lshift(v1, -shift) : rshift(v1, shift);
        default: break;
        }
        tmp = (Obj *)int_from_bits(v1);
        op->v1 = tmp;
        result = tmp->obj->calc2(op);
        val_destroy(tmp);
        op->v1 = &v1->v;
        return result;
    default:
        if (op->op != &o_MEMBER) {
            return o2->obj->rcalc2(op);
        }
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Bits *v2 = (Bits *)op->v2;
    Obj *o1 = op->v1;
    Obj *tmp, *result;
    ssize_t val;
    switch (o1->obj->type) {
    case T_BOOL:
        {
            Bool *v1 = (Bool *)o1;
            tmp = (Obj *)ref_bits(bits_value[v1->boolean]);
            op->v1 = tmp;
            result = calc2(op);
            val_destroy(tmp);
            op->v1 = &v1->v;
            return result;
        }
    case T_BITS:
        {
            Bits *v1 = (Bits *)o1;
            switch (op->op->op) {
            case O_CMP:
                val = icmp(v1, v2);
                if (val < 0) return (Obj *)ref_int(minus1_value);
                return (Obj *)ref_int(int_value[val > 0]);
            case O_EQ: return truth_reference(icmp(v1, v2) == 0);
            case O_NE: return truth_reference(icmp(v1, v2) != 0);
            case O_LT: return truth_reference(icmp(v1, v2) < 0);
            case O_LE: return truth_reference(icmp(v1, v2) <= 0);
            case O_GT: return truth_reference(icmp(v1, v2) > 0);
            case O_GE: return truth_reference(icmp(v1, v2) >= 0);
            case O_AND: return and_(v1, v2);
            case O_OR: return or_(v1, v2);
            case O_XOR: return xor_(v1, v2);
            case O_CONCAT: return concat(v1, v2);
            case O_IN: return obj_oper_error(op); /* TODO */
            default: break;
            }
            /* fall through */
        }
    case T_INT:
        tmp = (Obj *)int_from_bits(v2);
        op->v2 = tmp;
        result = o1->obj->calc2(op);
        val_destroy(tmp);
        op->v2 = &v2->v;
        return result;
    default: break;
    }
    return obj_oper_error(op);
}

void bitsobj_init(void) {
    new_type(&obj, T_BITS, "bits", sizeof(Bits));
    obj_init(&obj);
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
    obj.len = len;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;

    null_bits = new_bits(0);
    null_bits->len = 0;
    null_bits->bits = 0;
    null_bits->data[0] = 0;
    inv_bits = new_bits(0);
    inv_bits->len = ~0;
    inv_bits->bits = 0;
    inv_bits->data[0] = 0;
    bits_value[0] = new_bits(0);
    bits_value[0]->len = 0;
    bits_value[0]->bits = 1;
    bits_value[0]->data[0] = 0;
    bits_value[1] = new_bits(1);
    bits_value[1]->len = 1;
    bits_value[1]->bits = 1;
    bits_value[1]->data[0] = 1;
}

void bitsobj_names(void) {
    new_builtin("bits", val_reference(&BITS_OBJ->v));
}

void bitsobj_destroy(void) {
#ifdef DEBUG
    if (null_bits->v.refcount != 1) fprintf(stderr, "bits %" PRIuSIZE "\n", null_bits->v.refcount - 1);
    if (inv_bits->v.refcount != 1) fprintf(stderr, "invbits %" PRIuSIZE "\n", inv_bits->v.refcount - 1);
    if (bits_value[0]->v.refcount != 1) fprintf(stderr, "bit0 %" PRIuSIZE "\n", bits_value[0]->v.refcount - 1);
    if (bits_value[1]->v.refcount != 1) fprintf(stderr, "bit1 %" PRIuSIZE "\n", bits_value[1]->v.refcount - 1);
#endif

    val_destroy(&null_bits->v);
    val_destroy(&inv_bits->v);
    val_destroy(&bits_value[0]->v);
    val_destroy(&bits_value[1]->v);
}
