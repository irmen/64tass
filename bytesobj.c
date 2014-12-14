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
#include "bytesobj.h"
#include "eval.h"
#include "unicode.h"
#include "encoding.h"

#include "boolobj.h"
#include "floatobj.h"

static struct obj_s obj;

obj_t BYTES_OBJ = &obj;

static inline size_t byteslen(const value_t v1) {
    ssize_t len = v1->u.bytes.len;
    return (len < 0) ? ~len : len;
}

static MUST_CHECK value_t bytes_from_bits(const value_t);
static MUST_CHECK value_t bytes_from_int(const value_t);

static MUST_CHECK value_t create(const value_t v1, linepos_t epoint) {
    value_t err, ret;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_BYTES: return val_reference(v1);
    case T_BOOL: return bytes_from_u8(v1->u.boolean);
    case T_BITS: return bytes_from_bits(v1);
    case T_STR: return bytes_from_str(v1, epoint);
    case T_INT: return bytes_from_int(v1);
    case T_CODE: return bytes_from_code(v1, epoint);
    case T_FLOAT: 
         err = int_from_float(v1);
         ret = bytes_from_int(err);
         val_destroy(err);
         return ret;
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return val_reference(none_value);
}

static void destroy(value_t v1) {
    if (v1->u.bytes.val != v1->u.bytes.data) free(v1->u.bytes.data);
}

static uint8_t *bnew(value_t v, size_t len) {
    if (len > sizeof(v->u.bytes.val)) {
        uint8_t *s = (uint8_t *)malloc(len);
        if (!s) err_msg_out_of_memory();
        return s;
    }
    return v->u.bytes.val;
}

static MUST_CHECK value_t invert(const value_t v1) {
    size_t sz;
    sz = byteslen(v1);
    if (sz) {
        value_t v = val_alloc(BYTES_OBJ);
        v->u.bytes.len = ~v1->u.bytes.len;
        v->u.bytes.data = bnew(v, sz);
        memcpy(v->u.bytes.data, v1->u.bytes.data, sz);
        return v;
    } 
    return val_reference((v1->u.bytes.len < 0) ? null_bytes : inv_bytes);
}

static int same(const value_t v1, const value_t v2) {
    return v2->obj == BYTES_OBJ && v1->u.bytes.len == v2->u.bytes.len && (
            v1->u.bytes.data == v2->u.bytes.data ||
            !memcmp(v1->u.bytes.data, v2->u.bytes.data, v2->u.bytes.len));
}

static MUST_CHECK value_t truth(const value_t v1, enum truth_e type, linepos_t epoint) {
    value_t v;
    size_t i, sz;
    uint8_t inv;
    switch (type) {
    case TRUTH_ALL:
        sz = byteslen(v1);
        inv = -(v1->u.bytes.len < 0);
        for (i = 0; i < sz; i++) {
            if (v1->u.bytes.data[i] == inv) return val_reference(false_value);
        }
        return val_reference(true_value);
    case TRUTH_ANY:
    case TRUTH_BOOL:
        if (v1->u.bytes.len < 0) return val_reference(true_value);
        sz = byteslen(v1);
        for (i = 0; i < sz; i++) {
            if (v1->u.bytes.data[i]) return val_reference(true_value);
        }
        return val_reference(false_value);
    default: 
        v = new_error_obj(ERROR_____CANT_BOOL, epoint);
        v->u.error.u.objname = v1->obj->name;
        return v;
    }
}

static MUST_CHECK value_t repr(const value_t v1, linepos_t UNUSED(epoint)) {
    static const char *hex = "0123456789abcdef";
    size_t i, len, len2, sz;
    uint8_t *s, b;
    value_t v;
    v = val_alloc(STR_OBJ);
    sz = byteslen(v1);
    len2 = sz * 2;
    len = 8 + len2 + (v1->u.bytes.len < 0);
    if (len < len2 || sz > SIZE_MAX / 2) err_msg_out_of_memory(); /* overflow */
    s = str_create_elements(v, len);

    memcpy(s, "bytes(", 6);
    len = 6;
    if (v1->u.bytes.len < 0) {
        s[len++] = '~';
    }
    s[len++] = '$';
    for (i = 0;i < sz; i++) {
        b = v1->u.bytes.data[sz - i - 1];
        s[len++] = hex[b >> 4];
        s[len++] = hex[b & 0xf];
    }
    s[len++] = ')';
    v->u.str.len = len;
    v->u.str.chars = len;
    v->u.str.data = s;
    return v;
}

static MUST_CHECK value_t hash(const value_t v1, int *hs, linepos_t UNUSED(epoint)) {
    size_t l = byteslen(v1);
    const uint8_t *s2 = v1->u.bytes.data;
    unsigned int h;
    if (!l) {
        *hs = 0;
        return NULL;
    }
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= v1->u.bytes.len;
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

MUST_CHECK value_t bytes_from_str(const value_t v1, linepos_t epoint) {
    size_t len = v1->u.str.len, len2 = 0;
    uint8_t *s;
    value_t v;
    if (len) {
        if (actual_encoding) {
            int ch;
            if (len < sizeof(v->u.bytes.val)) len = sizeof(v->u.bytes.val);
            v = val_alloc(BYTES_OBJ);
            s = bnew(v, len);
            encode_string_init(v1, epoint);
            while ((ch = encode_string()) != EOF) {
                if (len2 >= len) {
                    if (v->u.bytes.val == s) {
                        len = 32;
                        s = (uint8_t *)malloc(len);
                        memcpy(s, v->u.bytes.val, len2);
                    } else {
                        len += 1024;
                        if (len < 1024) err_msg_out_of_memory(); /* overflow */
                        s = (uint8_t *)realloc(s, len);
                    }
                    if (!s) err_msg_out_of_memory();
                }
                s[len2++] = ch;
            }
        } else if (v1->u.str.chars == 1) {
            uint32_t ch2 = v1->u.str.data[0];
            if (ch2 & 0x80) utf8in(v1->u.str.data, &ch2);
            v = val_alloc(BYTES_OBJ);
            s = bnew(v, 3);
            s[0] = ch2;
            s[1] = ch2 >> 8;
            s[2] = ch2 >> 16;
            len2 = 3;
        } else {
            return new_error_obj(ERROR_BIG_STRING_CO, epoint);
        }
        if (v->u.bytes.val != s) {
            if (len2 <= sizeof(v->u.bytes.val)) {
                if (len2) memcpy(v->u.bytes.val, s, len2);
                free(s);
                s = v->u.bytes.val;
            } else if (len2 < len) {
                s = (uint8_t *)realloc(s, len2);
                if (!s) err_msg_out_of_memory();
            }
        }
        v->u.bytes.len = len2;
        v->u.bytes.data = s;
        return v;
    }
    return val_reference(null_bytes);
}

MUST_CHECK value_t bytes_from_u8(uint8_t i) {
    value_t v = val_alloc(BYTES_OBJ);
    v->u.bytes.data = v->u.bytes.val;
    v->u.bytes.val[0] = i;
    v->u.bytes.len = 1;
    return v;
}

MUST_CHECK value_t bytes_from_u16(uint16_t i) {
    value_t v = val_alloc(BYTES_OBJ);
    v->u.bytes.data = v->u.bytes.val;
    v->u.bytes.val[0] = i;
    v->u.bytes.val[1] = i >> 8;
    v->u.bytes.len = 2;
    return v;
}

static MUST_CHECK value_t bytes_from_bits(const value_t v1) {
    size_t i, sz, len1;
    uint8_t *d;
    value_t v;
    int inv = v1->u.bits.len < 0;

    len1 = v1->u.bits.bits;
    if (!len1) {
        return val_reference(inv ? inv_bytes : null_bytes);
    }

    v = val_alloc(BYTES_OBJ);

    sz = len1 / 8;
    if (len1 % 8) sz++;
    v->u.bytes.data = d = bnew(v, sz);
    v->u.bytes.len = inv ? ~sz : sz;

    len1 = v1->u.bits.len;
    i = 0;
    if (len1) {
        bdigit_t b = v1->u.bits.data[0];
        int bits = 0;
        size_t j = 0;
        while (sz > i) {
            d[i++] = b >> bits;
            if (bits == (8 * sizeof(bdigit_t)) - 8) {
                bits = 0;
                j++;
                if (j >= len1) break;
                b = v1->u.bits.data[j];
            } else bits += 8;
        }
    }
    if (sz > i) memset(d + i , 0, sz - i);
    return v;
}

static MUST_CHECK value_t bytes_from_int(const value_t v1) {
    unsigned int inv;
    size_t i, j, sz, bits;
    uint8_t *d;
    const digit_t *b;
    value_t v;

    if (!v1->u.integer.len) return val_reference(null_bytes);
    if (v1->u.integer.len == -1 && v1->u.integer.data[0] == 1) return val_reference(inv_bytes);

    inv = v1->u.integer.len < 0;
    sz = inv ? -v1->u.integer.len : v1->u.integer.len;
    if (sz > SSIZE_MAX / sizeof(digit_t)) err_msg_out_of_memory(); /* overflow */
    sz *= sizeof(digit_t);
    v = val_alloc(BYTES_OBJ);
    d = bnew(v, sz);

    b = v1->u.integer.data;
    if (inv) {
        int c = !b[0];
        digit_t b2 = b[0] - 1;
        bits = j = 0;
        for (i = 0; i < sz; i++) {
            d[i] = b2 >> bits;
            if (bits == (8 * sizeof(digit_t)) - 8) {
                j++;
                if (c) {
                    c = !b[j];
                    b2 = b[j] - 1;
                } else b2 = b[j];
                bits = 0;
            } else bits += 8;
        }
    } else {
        digit_t b2 = b[0];
        bits = j = 0;
        for (i = 0; i < sz; i++) {
            d[i] = b2 >> bits;
            if (bits == (8 * sizeof(digit_t)) - 8) {
                j++;
                b2 = b[j];
                bits = 0;
            } else bits += 8;
        }
    }

    while (sz && !d[sz - 1]) sz--;
    if (v->u.bytes.val != d) {
        if (sz <= sizeof(v->u.bytes.val)) {
            if (sz) memcpy(v->u.bytes.val, d, sz);
            free(d);
            d = v->u.bytes.val;
        } else if (sz < i) {
            d = (uint8_t *)realloc(d, sz);
            if (!d) err_msg_out_of_memory();
        }
    }
    v->u.bytes.data = d;
    v->u.bytes.len = inv ? ~sz : sz;

    return v;
}

static MUST_CHECK value_t ival(const value_t v1, ival_t *iv, int bits, linepos_t epoint) {
    value_t v;
    switch (byteslen(v1)) {
    case 0: *iv = -(v1->u.bytes.len < 0); return NULL;
    case 1: *iv = v1->u.bytes.data[0];
            if (bits < 8) break;
            if (v1->u.bytes.len < 0) *iv = ~*iv;
            return NULL;
    case 2: *iv = (v1->u.bytes.data[1] << 8) + v1->u.bytes.data[0];
            if (bits < 16) break;
            if (v1->u.bytes.len < 0) *iv = ~*iv;
            return NULL;
    case 3: *iv = (v1->u.bytes.data[2] << 16) + (v1->u.bytes.data[1] << 8) + v1->u.bytes.data[0];
            if (bits < 24) break;
            if (v1->u.bytes.len < 0) *iv = ~*iv;
            return NULL;
    case 4: *iv = (v1->u.bytes.data[3] << 24) + (v1->u.bytes.data[2] << 16) + (v1->u.bytes.data[1] << 8) + v1->u.bytes.data[0];
            if (bits < 32) break;
            if (v1->u.bytes.len < 0) *iv = ~*iv;
            return NULL;
    default: break;
    }
    v = new_error_obj(ERROR_____CANT_IVAL, epoint);
    v->u.error.u.bits = bits;
    return v;
}

static MUST_CHECK value_t uval(const value_t v1, uval_t *uv, int bits, linepos_t epoint) {
    value_t v;
    switch (byteslen(v1)) {
    case 0: *uv = -(v1->u.bytes.len < 0); return NULL;
    case 1: *uv = v1->u.bytes.data[0];
            if (bits < 8) break;
            if (v1->u.bytes.len < 0) *uv = ~*uv;
            return NULL;
    case 2: *uv = (v1->u.bytes.data[1] << 8) + v1->u.bytes.data[0];
            if (bits < 16) break;
            if (v1->u.bytes.len < 0) *uv = ~*uv;
            return NULL;
    case 3: *uv = (v1->u.bytes.data[2] << 16) + (v1->u.bytes.data[1] << 8) + v1->u.bytes.data[0];
            if (bits < 24) break;
            if (v1->u.bytes.len < 0) *uv = ~*uv;
            return NULL;
    case 4: *uv = (v1->u.bytes.data[3] << 24) + (v1->u.bytes.data[2] << 16) + (v1->u.bytes.data[1] << 8) + v1->u.bytes.data[0];
            if (bits < 32) break;
            if (v1->u.bytes.len < 0) *uv = ~*uv;
            return NULL;
    default: break;
    }
    v = new_error_obj(ERROR_____CANT_UVAL, epoint);
    v->u.error.u.bits = bits;
    return v;
}

MUST_CHECK value_t float_from_bytes(const value_t v1, linepos_t epoint) {
    double d = -(v1->u.bytes.len < 0);
    size_t i, len1 = d ? ~v1->u.bytes.len : v1->u.bytes.len;
    for (i = 0; i < len1; i++) {
        if (v1->u.bytes.len < 0) d -= ldexp((double)v1->u.bytes.data[i], i * 8);
        else d += ldexp((double)v1->u.bytes.data[i], i * 8);
        if (d == HUGE_VAL || d == -HUGE_VAL) {
            return new_error_obj(ERROR_NUMERIC_OVERF, epoint);
        }
    }
    return float_from_double(d);
}

static MUST_CHECK value_t sign(const value_t v1, linepos_t UNUSED(epoint)) {
    size_t i, sz;
    if (v1->u.bytes.len < 0) return int_from_int(-1);
    sz = byteslen(v1);
    for (i = 0; i < sz; i++) {
        if (v1->u.bytes.data[i]) return val_reference(int_value[1]);
    }
    return val_reference(int_value[0]);
}

static MUST_CHECK value_t absolute(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_bytes(v1);
}

static MUST_CHECK value_t len(const value_t v1, linepos_t UNUSED(epoint)) {
    return int_from_size(byteslen(v1));
}

static MUST_CHECK value_t getiter(value_t v1) {
    value_t v = val_alloc(ITER_OBJ);
    v->u.iter.val = 0;
    v->u.iter.iter = &v->u.iter.val;
    v->u.iter.data = val_reference(v1);
    return v;
}

static MUST_CHECK value_t next(value_t v1) {
    const value_t vv1 = v1->u.iter.data;
    value_t v;
    if (v1->u.iter.val >= byteslen(vv1)) return NULL;
    v = val_alloc(BYTES_OBJ);
    v->u.bytes.val[0] = (-(vv1->u.bytes.len < 0)) ^ vv1->u.bytes.data[v1->u.iter.val++];
    v->u.bytes.len = 1;
    v->u.bytes.data = v->u.bytes.val;
    return v;
}

static MUST_CHECK value_t and_(value_t vv1, value_t vv2) {
    size_t i, len1, len2, sz;
    int neg1, neg2;
    uint8_t *v1, *v2, *v;
    value_t vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->u.bytes.len < 0; neg2 = vv2->u.bytes.len < 0;

    sz = neg2 ? len1 : len2;
    if (!sz) return val_reference((neg1 & neg2) ? inv_bytes : null_bytes);
    vv = val_alloc(BYTES_OBJ);
    v = bnew(vv, sz);
    v1 = vv1->u.bytes.data; v2 = vv2->u.bytes.data;

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
    vv->u.bytes.len = (neg1 & neg2) ? ~sz : sz;
    vv->u.bytes.data = v;
    return vv;
}

static MUST_CHECK value_t or_(value_t vv1, value_t vv2) {
    size_t i, len1, len2, sz;
    int neg1, neg2;
    uint8_t *v1, *v2, *v;
    value_t vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->u.bytes.len < 0; neg2 = vv2->u.bytes.len < 0;

    sz = neg2 ? len2 : len1;
    if (!sz) return val_reference((neg1 | neg2) ? inv_bytes : null_bytes);
    vv = val_alloc(BYTES_OBJ);
    v = bnew(vv, sz);
    v1 = vv1->u.bytes.data; v2 = vv2->u.bytes.data;

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

    vv->u.bytes.len = (neg1 | neg2) ? ~sz : sz;
    vv->u.bytes.data = v;
    return vv;
}

static MUST_CHECK value_t xor_(value_t vv1, value_t vv2) {
    size_t i, len1, len2, sz;
    int neg1, neg2;
    uint8_t *v1, *v2, *v;
    value_t vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const value_t tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->u.bytes.len < 0; neg2 = vv2->u.bytes.len < 0;

    sz = len1;
    if (!sz) return val_reference((neg1 ^ neg2) ? inv_bytes : null_bytes);
    vv = val_alloc(BYTES_OBJ);
    v = bnew(vv, sz);
    v1 = vv1->u.bytes.data; v2 = vv2->u.bytes.data;

    for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
    for (; i < len1; i++) v[i] = v1[i];

    vv->u.bytes.len = (neg1 ^ neg2) ? ~sz : sz;
    vv->u.bytes.data = v;
    return vv;
}

static MUST_CHECK value_t concat(value_t v1, value_t v2) {
    value_t v;
    uint8_t *s;
    int inv;
    size_t ln, i, len1, len2;

    if (!v1->u.bytes.len) {
        return val_reference(v2);
    }
    if (v2->u.bytes.len == 0 || v2->u.bytes.len == ~(ssize_t)0) {
        return val_reference(v1);
    }
    len1 = byteslen(v1);
    len2 = byteslen(v2);
    ln = len1 + len2;
    if (ln < len2) err_msg_out_of_memory(); /* overflow */

    v = val_alloc(BYTES_OBJ);
    s = bnew(v, ln);
    inv = (v2->u.bytes.len ^ v1->u.bytes.len) < 0;

    memcpy(s, v1->u.bytes.data, len1);
    if (inv) {
        for (i = 0; i < len2; i++) s[i + len1] = ~v2->u.bytes.data[i];
    } else memcpy(s + len1, v2->u.bytes.data, len2);
    v->u.bytes.len = (v1->u.bytes.len < 0) ? ~ln : ln;
    v->u.bytes.data = s;
    return v;
}

static int icmp(value_t v1, value_t v2) {
    size_t len1 = byteslen(v1), len2 = byteslen(v2);
    int h = memcmp(v1->u.bytes.data, v2->u.bytes.data, (len1 < len2) ? len1 : len2);
    if (h) return h;
    return (len1 > len2) - (len1 < len2);
}

static MUST_CHECK value_t calc1(oper_t op) {
    value_t v1 = op->v1, v;
    value_t tmp;
    switch (op->op->u.oper.op) {
    case O_BANK: 
        if (v1->u.bytes.len > 2) return bytes_from_u8(v1->u.bytes.data[2]);
        if (v1->u.bytes.len < ~2) return bytes_from_u8(~v1->u.bytes.data[2]);
        return bytes_from_u8(-(v1->u.bytes.len < 0));
    case O_HIGHER: 
        if (v1->u.bytes.len > 1) return bytes_from_u8(v1->u.bytes.data[1]);
        if (v1->u.bytes.len < ~1) return bytes_from_u8(~v1->u.bytes.data[1]);
        return bytes_from_u8(-(v1->u.bytes.len < 0));
    case O_LOWER: 
        if (v1->u.bytes.len > 0) return bytes_from_u8(v1->u.bytes.data[0]);
        if (v1->u.bytes.len < ~0) return bytes_from_u8(~v1->u.bytes.data[0]);
        return bytes_from_u8(-(v1->u.bytes.len < 0));
    case O_HWORD:
        if (v1->u.bytes.len > 2) return bytes_from_u16(v1->u.bytes.data[1] + (v1->u.bytes.data[2] << 8));
        if (v1->u.bytes.len > 1) return bytes_from_u16(v1->u.bytes.data[1]);
        if (v1->u.bytes.len < ~2) return bytes_from_u16(~(v1->u.bytes.data[1] + (v1->u.bytes.data[2] << 8)));
        if (v1->u.bytes.len < ~1) return bytes_from_u16(~v1->u.bytes.data[1]);
        return bytes_from_u16(-(v1->u.bytes.len < 0));
    case O_WORD:
        if (v1->u.bytes.len > 1) return bytes_from_u16(v1->u.bytes.data[0] + (v1->u.bytes.data[1] << 8));
        if (v1->u.bytes.len > 0) return bytes_from_u16(v1->u.bytes.data[0]);
        if (v1->u.bytes.len < ~1) return bytes_from_u16(~(v1->u.bytes.data[0] + (v1->u.bytes.data[1] << 8)));
        if (v1->u.bytes.len < ~0) return bytes_from_u16(~v1->u.bytes.data[0]);
        return bytes_from_u16(-(v1->u.bytes.len < 0));
    case O_BSWORD:
        if (v1->u.bytes.len > 1) return bytes_from_u16(v1->u.bytes.data[1] + (v1->u.bytes.data[0] << 8));
        if (v1->u.bytes.len > 0) return bytes_from_u16(v1->u.bytes.data[0] << 8);
        if (v1->u.bytes.len < ~1) return bytes_from_u16(~(v1->u.bytes.data[1] + (v1->u.bytes.data[0] << 8)));
        if (v1->u.bytes.len < ~0) return bytes_from_u16(~(v1->u.bytes.data[0] << 8));
        return bytes_from_u16(-(v1->u.bytes.len < 0));
    case O_INV: return invert(v1);
    case O_NEG:
    case O_POS:
    case O_STRING: tmp = int_from_bytes(v1);break;
    default: return obj_oper_error(op);
    }
    op->v1 = tmp;
    v = tmp->obj->calc1(op);
    op->v1 = v1;
    val_destroy(tmp);
    return v;
}

static MUST_CHECK value_t calc2_bytes(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    int val;
    switch (op->op->u.oper.op) {
    case O_ADD:
    case O_SUB:
    case O_MUL:
    case O_DIV:
    case O_MOD:
    case O_EXP:
        {
            value_t tmp, tmp2, result;
            tmp = int_from_bytes(v1);
            tmp2 = int_from_bytes(v2);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = v1;
            op->v2 = v2;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_AND: return and_(v1, v2);
    case O_OR: return or_(v1, v2);
    case O_XOR: return xor_(v1, v2);
    case O_LSHIFT:
    case O_RSHIFT:
        {
            value_t tmp, tmp2, result;
            tmp = bits_from_bytes(v1);
            tmp2 = bits_from_bytes(v2);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = v1;
            op->v2 = v2;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
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
    case O_CONCAT: return concat(v1, v2);
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            size_t len1 = byteslen(v1), len2 = byteslen(v2), i;
            if (!len1) return val_reference(true_value);
            if (len1 > len2) return val_reference(false_value);
            c2 = v2->u.bytes.data;
            e = c2 + len2 - len1;
            if ((v1->u.bytes.len ^ v2->u.bytes.len) < 0) {
                for (;;) {
                    c = (uint8_t *)memchr(c2, ~v1->u.bytes.data[0], e - c2 + 1);
                    if (!c) return val_reference(false_value);
                    for (i = 1; i < len1; i++) {
                        if (c[i] != (0xff - v1->u.bytes.data[i])) break;
                    }
                    if (i == len1) return val_reference(true_value);
                    c2 = c + 1;
                }
            } else {
                for (;;) {
                    c = (uint8_t *)memchr(c2, v1->u.bytes.data[0], e - c2 + 1);
                    if (!c) return val_reference(false_value);
                    if (!memcmp(c, v1->u.bytes.data, len1)) return val_reference(true_value);
                    c2 = c + 1;
                }
            }
        }
    default: break;
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK value_t repeat(oper_t op) {
    value_t v1 = op->v1, v;
    uval_t rep;
    size_t len1 = byteslen(v1);

    v = op->v2->obj->uval(op->v2, &rep, 8*sizeof(uval_t), op->epoint2);
    if (v) return v;

    if (len1 && rep) {
        uint8_t *s, *s2;
        if (rep == 1) {
            return val_reference(v1);
        }
        if (len1 > SSIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
        v = val_alloc(BYTES_OBJ);
        v->u.bytes.data = s2 = s = bnew(v, len1 * rep);
        v->u.bytes.len = 0;
        while (rep--) {
            memcpy(s + v->u.bytes.len, v1->u.bytes.data, len1);
            v->u.bytes.len += len1;
        }
        if (v1->u.bytes.len < 0) v->u.bytes.len = ~v->u.bytes.len;
        return v;
    }
    return val_reference((v1->u.bytes.len < 0) ? inv_bytes : null_bytes);
}

static inline MUST_CHECK value_t slice(value_t v2, oper_t op, size_t ln) {
    uint8_t *p, *p2, inv;
    value_t v, v1 = op->v1;
    size_t length;
    ival_t offs, end, step;

    v = sliceparams(v2, ln, &length, &offs, &end, &step, op->epoint2);
    if (v) return v;

    if (!length) {
        return val_reference(null_bytes);
    }
    inv = v1->u.bytes.len < 0;
    if (step == 1 && !inv) {
        if (length == byteslen(v1)) {
            return val_reference(v1); /* original bytes */
        }
        v = val_alloc(BYTES_OBJ);
        p = p2 = bnew(v, length);
        memcpy(p2, v1->u.bytes.data + offs, length);
    } else {
        inv = -inv;
        v = val_alloc(BYTES_OBJ);
        p = p2 = bnew(v, length);
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            *p2++ = v1->u.bytes.data[offs] ^ inv;
            offs += step;
        }
    }
    v->u.bytes.len = length;
    v->u.bytes.data = p;
    return v;
}

static inline MUST_CHECK value_t iindex(oper_t op) {
    uint8_t *p2;
    size_t offs, len1, len2;
    size_t i;
    value_t v1 = op->v1, v2 = op->v2, v, err;
    uint8_t inv;

    if (v2->u.funcargs.len != 1) {
        err_msg_argnum(v2->u.funcargs.len, 1, 1, op->epoint2);
        return val_reference(none_value);
    }
    v2 = v2->u.funcargs.val->val;

    len1 = byteslen(v1);

    if (v2->obj == LIST_OBJ) {
        if (!v2->u.list.len) {
            return val_reference(null_bytes);
        }
        len2 = v2->u.list.len;
        inv = -(v1->u.bytes.len < 0);
        v = val_alloc(BYTES_OBJ);
        v->u.bytes.data = p2 = bnew(v, len2);
        for (i = 0; i < len2; i++) {
            err = indexoffs(v2->u.list.data[i], len1, &offs, op->epoint2);
            if (err) {
                val_destroy(v);
                return err;
            }
            *p2++ = v1->u.bytes.data[offs] ^ inv;
        }
        v->u.bytes.len = i;
        return v;
    }
    if (v2->obj == COLONLIST_OBJ) {
        return slice(v2, op, len1);
    }
    err = indexoffs(v2, len1, &offs, op->epoint2);
    if (err) return err;
    inv = -(v1->u.bytes.len < 0);
    v = val_alloc(BYTES_OBJ);
    v->u.bytes.len = 1;
    v->u.bytes.val[0] = v1->u.bytes.data[offs] ^ inv;
    v->u.bytes.data = v->u.bytes.val;
    return v;
}

static MUST_CHECK value_t calc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    value_t tmp;

    if (op->op == &o_INDEX) {
        return iindex(op);
    }
    if (op->op == &o_X) {
        return repeat(op); 
    }
    switch (v2->obj->type) {
    case T_BYTES: return calc2_bytes(op);
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            value_t result;
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR:
            case O_LSHIFT:
            case O_RSHIFT: tmp = bits_from_bytes(v1); break;
            default: tmp = int_from_bytes(v1);
            }
            op->v1 = tmp;
            result = tmp->obj->calc2(op);
            op->v1 = v1;
            val_destroy(tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
    case T_STR:
    case T_GAP:
    case T_REGISTER:
    case T_DICT:
    case T_NONE:
    case T_ERROR:
        if (op->op != &o_MEMBER) {
            return v2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK value_t rcalc2(oper_t op) {
    value_t v1 = op->v1, v2 = op->v2;
    value_t tmp;
    switch (v1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            value_t result;
            switch (op->op->u.oper.op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR: tmp = bits_from_bytes(v2); break;
            default: tmp = int_from_bytes(v2);
            }
            op->v2 = tmp;
            result = v1->obj->calc2(op);
            op->v2 = v2;
            val_destroy(tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
    case T_STR:
    case T_NONE:
    case T_ERROR:
        if (op->op != &o_IN) {
            return v1->obj->calc2(op);
        }
    default: break;
    }
    return obj_oper_error(op); 
}

void bytesobj_init(void) {
    obj_init(&obj, T_BYTES, "bytes");
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
    obj.getiter = getiter;
    obj.next = next;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
}
