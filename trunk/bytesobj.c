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
#include "bytesobj.h"
#include "eval.h"
#include "unicode.h"
#include "encoding.h"

#include "boolobj.h"
#include "floatobj.h"
#include "codeobj.h"
#include "intobj.h"
#include "strobj.h"
#include "bitsobj.h"
#include "listobj.h"
#include "error.h"

static struct obj_s obj;

obj_t BYTES_OBJ = &obj;
Bytes *null_bytes;
Bytes *inv_bytes;
static Bytes *bytes_value[256];

static inline size_t byteslen(const Bytes *v1) {
    ssize_t len = v1->len;
    return (len < 0) ? ~len : len;
}

static MUST_CHECK Bytes *bytes_from_bits(const Bits *);
static MUST_CHECK Bytes *bytes_from_int(const Int *);

static MUST_CHECK Obj *create(Obj *v1, linepos_t epoint) {
    Obj *err, *ret;
    switch (v1->obj->type) {
    case T_NONE:
    case T_ERROR:
    case T_BYTES: return val_reference(v1);
    case T_BOOL: return (Obj *)bytes_from_u8(((Bool *)v1)->boolean);
    case T_BITS: return (Obj *)bytes_from_bits((Bits *)v1);
    case T_STR: return bytes_from_str((Str *)v1, epoint, BYTES_MODE_TEXT);
    case T_INT: return (Obj *)bytes_from_int((Int *)v1);
    case T_CODE: return bytes_from_code((Code *)v1, epoint);
    case T_FLOAT: 
         err = (Obj *)int_from_float((Float *)v1);
         if (err->obj != INT_OBJ) return err;
         ret = (Obj *)bytes_from_int((Int *)err);
         val_destroy(err);
         return ret;
    default: break;
    }
    err_msg_wrong_type(v1, NULL, epoint);
    return (Obj *)ref_none();
}

static void destroy(Obj *o1) {
    Bytes *v1 = (Bytes *)o1;
    if (v1->val != v1->data) free(v1->data);
}

static uint8_t *bnew(Bytes *v, size_t len) {
    if (len > sizeof(v->val)) {
        uint8_t *s = (uint8_t *)malloc(len);
        if (!s) err_msg_out_of_memory();
        return s;
    }
    return v->val;
}

static MUST_CHECK Obj *invert(const Bytes *v1) {
    size_t sz;
    sz = byteslen(v1);
    if (sz) {
        Bytes *v = new_bytes();
        v->len = ~v1->len;
        v->data = bnew(v, sz);
        memcpy(v->data, v1->data, sz);
        return &v->v;
    } 
    return (Obj *)ref_bytes((v1->len < 0) ? null_bytes : inv_bytes);
}

static int same(const Obj *o1, const Obj *o2) {
    const Bytes *v1 = (const Bytes *)o1, *v2 = (const Bytes *)o2;
    return o2->obj == BYTES_OBJ && v1->len == v2->len && (
            v1->data == v2->data ||
            !memcmp(v1->data, v2->data, byteslen(v2)));
}

static MUST_CHECK Obj *truth(Obj *o1, enum truth_e type, linepos_t UNUSED(epoint)) {
    const Bytes *v1 = (const Bytes *)o1;
    size_t i, sz;
    uint8_t inv;
    switch (type) {
    case TRUTH_ALL:
        sz = byteslen(v1);
        inv = -(v1->len < 0);
        for (i = 0; i < sz; i++) {
            if (v1->data[i] == inv) return (Obj *)ref_bool(false_value);
        }
        return (Obj *)ref_bool(true_value);
    case TRUTH_ANY:
    default:
        if (v1->len < 0) return (Obj *)ref_bool(true_value);
        sz = byteslen(v1);
        for (i = 0; i < sz; i++) {
            if (v1->data[i]) return (Obj *)ref_bool(true_value);
        }
        return (Obj *)ref_bool(false_value);
    }
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
    static const char *hex = "0123456789abcdef";
    size_t i, len, len2, sz;
    uint8_t *s, b;
    Str *v = new_str();
    sz = byteslen(v1);
    len2 = sz * 2;
    len = 8 + len2 + (v1->len < 0);
    if (len < len2 || sz > SIZE_MAX / 2) err_msg_out_of_memory(); /* overflow */
    s = str_create_elements(v, len);

    memcpy(s, "bytes(", 6);
    len = 6;
    if (v1->len < 0) {
        s[len++] = '~';
    }
    s[len++] = '$';
    for (i = 0;i < sz; i++) {
        b = v1->data[sz - i - 1];
        s[len++] = hex[b >> 4];
        s[len++] = hex[b & 0xf];
    }
    s[len++] = ')';
    v->len = len;
    v->chars = len;
    v->data = s;
    return &v->v;
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
    size_t l = byteslen(v1);
    const uint8_t *s2 = v1->data;
    unsigned int h;
    if (!l) {
        *hs = 0;
        return NULL;
    }
    h = *s2 << 7;
    while (l--) h = (1000003 * h) ^ *s2++;
    h ^= v1->len;
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

MUST_CHECK Obj *bytes_from_str(const Str *v1, linepos_t epoint, enum bytes_mode_e mode) {
    size_t len = v1->len, len2 = (mode == BYTES_MODE_PTEXT || mode == BYTES_MODE_NULL);
    uint8_t *s;
    Bytes *v;
    if (len || len2) {
        if (actual_encoding) {
            int ch;
            if (len < sizeof(v->val)) len = sizeof(v->val);
            if (!len) {
                return (Obj *)ref_bytes(null_bytes);
            }
            v = new_bytes();
            s = bnew(v, len);
            encode_string_init(v1, epoint);
            while ((ch = encode_string()) != EOF) {
                if (len2 >= len) {
                    if (v->val == s) {
                        len = 32;
                        s = (uint8_t *)malloc(len);
                        memcpy(s, v->val, len2);
                    } else {
                        len += 1024;
                        if (len < 1024) err_msg_out_of_memory(); /* overflow */
                        s = (uint8_t *)realloc(s, len);
                    }
                    if (!s) err_msg_out_of_memory();
                }
                switch (mode) {
                case BYTES_MODE_SHIFT_CHECK:
                case BYTES_MODE_SHIFT: if (ch & 0x80) {encode_error(ERROR___NO_HIGH_BIT); ch &= 0x7f;} s[len2] = ch; break;
                case BYTES_MODE_SHIFTL: if (ch & 0x80) encode_error(ERROR___NO_HIGH_BIT); s[len2] = ch << 1; break;
                case BYTES_MODE_NULL_CHECK:if (!ch) {encode_error(ERROR_NO_ZERO_VALUE); ch = 0xff;} s[len2] = ch; break;
                case BYTES_MODE_NULL: if (!ch) encode_error(ERROR_NO_ZERO_VALUE); s[len2 - 1] = ch; break;
                case BYTES_MODE_PTEXT:
                case BYTES_MODE_TEXT: s[len2] = ch; break;
                }
                len2++;
            }
            switch (mode) {
            case BYTES_MODE_SHIFT: if (len2) s[len2 - 1] |= 0x80; else err_msg2(ERROR__BYTES_NEEDED, NULL, epoint); break;
            case BYTES_MODE_SHIFTL: if (len2) s[len2 - 1] |= 0x01; else err_msg2(ERROR__BYTES_NEEDED, NULL, epoint);break;
            case BYTES_MODE_NULL: s[len2 - 1] = 0x00; break;
            case BYTES_MODE_PTEXT: s[0] = len2 - 1; if (len2 > 256) err_msg2(ERROR____PTEXT_LONG, &len2, epoint); break;
            case BYTES_MODE_SHIFT_CHECK:
            case BYTES_MODE_NULL_CHECK:
            case BYTES_MODE_TEXT: break;
            }
        } else if (v1->chars == 1) {
            uint32_t ch2 = v1->data[0];
            if (ch2 & 0x80) utf8in(v1->data, &ch2);
            return (Obj *)bytes_from_uval(ch2, 3);
        } else {
            return (Obj *)new_error(ERROR_BIG_STRING_CO, epoint);
        }
        if (v->val != s) {
            if (len2 <= sizeof(v->val)) {
                if (len2) memcpy(v->val, s, len2);
                free(s);
                s = v->val;
            } else if (len2 < len) {
                s = (uint8_t *)realloc(s, len2);
                if (!s) err_msg_out_of_memory();
            }
        }
        v->len = len2;
        v->data = s;
        return &v->v;
    }
    if (actual_encoding) {
        if (mode == BYTES_MODE_SHIFT || mode == BYTES_MODE_SHIFTL) err_msg2(ERROR__EMPTY_STRING, NULL, epoint);
    }
    return (Obj *)ref_bytes(null_bytes);
}

MUST_CHECK Bytes *bytes_from_u8(uint8_t i) {
    Bytes *v = bytes_value[i];
    if (!v) {
        v = new_bytes();
        v->data = v->val;
        v->val[0] = i;
        v->len = 1;
        bytes_value[i] = v;
    }
    return ref_bytes(v);
}

MUST_CHECK Bytes *bytes_from_u16(uint16_t i) {
    Bytes *v = new_bytes();
    v->data = v->val;
    v->val[0] = i;
    v->val[1] = i >> 8;
    v->len = 2;
    return v;
}

MUST_CHECK Bytes *bytes_from_uval(uval_t i, int bytes) {
    Bytes *v = new_bytes();
    v->data = v->val;
    v->val[0] = i;
    v->val[1] = i >> 8;
    v->val[2] = i >> 16;
    v->val[3] = i >> 24;
    v->len = bytes;
    return v;
}

static MUST_CHECK Bytes *bytes_from_bits(const Bits *v1) {
    size_t i, sz, len1;
    uint8_t *d;
    Bytes *v;
    int inv = v1->len < 0;

    len1 = v1->bits;
    if (!len1) {
        return ref_bytes(inv ? inv_bytes : null_bytes);
    }
    if (len1 <= 8 && !inv) {
        return bytes_from_u8(v1->data[0]);
    }

    v = new_bytes();

    sz = len1 / 8;
    if (len1 % 8) sz++;
    v->data = d = bnew(v, sz);
    v->len = inv ? ~sz : sz;

    len1 = v1->len;
    i = 0;
    if (len1) {
        bdigit_t b = v1->data[0];
        int bits = 0;
        size_t j = 0;
        while (sz > i) {
            d[i++] = b >> bits;
            if (bits == (8 * sizeof(bdigit_t)) - 8) {
                bits = 0;
                j++;
                if (j >= len1) break;
                b = v1->data[j];
            } else bits += 8;
        }
    }
    if (sz > i) memset(d + i , 0, sz - i);
    return v;
}

static MUST_CHECK Bytes *bytes_from_int(const Int *v1) {
    unsigned int inv;
    size_t i, j, sz, bits;
    uint8_t *d;
    const digit_t *b;
    Bytes *v;

    switch (v1->len) {
    case -1:
        if (v1->data[0] == 1) return ref_bytes(inv_bytes);
    case 0:
        return ref_bytes(null_bytes);
    case 1:
        if (v1->data[0] < 256) return bytes_from_u8(v1->data[0]);
    default:
        break;
    }

    inv = v1->len < 0;
    sz = inv ? -v1->len : v1->len;
    if (sz > SSIZE_MAX / sizeof(digit_t)) err_msg_out_of_memory(); /* overflow */
    sz *= sizeof(digit_t);
    v = new_bytes();
    d = bnew(v, sz);

    b = v1->data;
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
    if (v->val != d) {
        if (sz <= sizeof(v->val)) {
            if (sz) memcpy(v->val, d, sz);
            free(d);
            d = v->val;
        } else if (sz < i) {
            d = (uint8_t *)realloc(d, sz);
            if (!d) err_msg_out_of_memory();
        }
    }
    v->data = d;
    v->len = inv ? ~sz : sz;

    return v;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, int bits, linepos_t epoint) {
    Bytes *v1 = (Bytes *)o1;
    Error *v;
    switch (byteslen(v1)) {
    case 0: *iv = -(v1->len < 0); return NULL;
    case 1: *iv = v1->data[0];
            if (bits < 8) break;
            if (v1->len < 0) *iv = ~*iv;
            return NULL;
    case 2: *iv = (v1->data[1] << 8) + v1->data[0];
            if (bits < 16) break;
            if (v1->len < 0) *iv = ~*iv;
            return NULL;
    case 3: *iv = (v1->data[2] << 16) + (v1->data[1] << 8) + v1->data[0];
            if (bits < 24) break;
            if (v1->len < 0) *iv = ~*iv;
            return NULL;
    case 4: *iv = (v1->data[3] << 24) + (v1->data[2] << 16) + (v1->data[1] << 8) + v1->data[0];
            if (bits < 32) break;
            if (v1->len < 0) *iv = ~*iv;
            return NULL;
    default: break;
    }
    v = new_error(ERROR_____CANT_IVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = val_reference(o1);
    return v;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, int bits, linepos_t epoint) {
    Bytes *v1 = (Bytes *)o1;
    Error *v;
    switch (byteslen(v1)) {
    case 0: *uv = -(v1->len < 0); return NULL;
    case 1: *uv = v1->data[0];
            if (bits < 8) break;
            if (v1->len < 0) *uv = ~*uv;
            return NULL;
    case 2: *uv = (v1->data[1] << 8) + v1->data[0];
            if (bits < 16) break;
            if (v1->len < 0) *uv = ~*uv;
            return NULL;
    case 3: *uv = (v1->data[2] << 16) + (v1->data[1] << 8) + v1->data[0];
            if (bits < 24) break;
            if (v1->len < 0) *uv = ~*uv;
            return NULL;
    case 4: *uv = (v1->data[3] << 24) + (v1->data[2] << 16) + (v1->data[1] << 8) + v1->data[0];
            if (bits < 32) break;
            if (v1->len < 0) *uv = ~*uv;
            return NULL;
    default: break;
    }
    v = new_error(ERROR_____CANT_UVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = val_reference(o1);
    return v;
}

MUST_CHECK Obj *float_from_bytes(const Bytes *v1, linepos_t epoint) {
    double d = -(v1->len < 0);
    size_t i, len1 = d ? ~v1->len : v1->len;
    for (i = 0; i < len1; i++) {
        if (v1->len < 0) d -= ldexp((double)v1->data[i], i * 8);
        else d += ldexp((double)v1->data[i], i * 8);
    }
    return float_from_double(d, epoint);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
    size_t i, sz;
    if (v1->len < 0) return (Obj *)ref_int(minus1_value);
    sz = byteslen(v1);
    for (i = 0; i < sz; i++) {
        if (v1->data[i]) return (Obj *)ref_int(int_value[1]);
    }
    return (Obj *)ref_int(int_value[0]);
}

static MUST_CHECK Obj *absolute(Obj *o1, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
    return (Obj *)int_from_bytes(v1);
}

static MUST_CHECK Obj *len(Obj *o1, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
    return (Obj *)int_from_size(byteslen(v1));
}

static MUST_CHECK Iter *getiter(Obj *v1) {
    Iter *v = (Iter *)val_alloc(ITER_OBJ);
    v->val = 0;
    v->iter = &v->val;
    v->data = val_reference(v1);
    return v;
}

static MUST_CHECK Obj *next(Iter *v1) {
    const Bytes *vv1 = (Bytes *)v1->data;
    if (v1->val >= byteslen(vv1)) return NULL;
    return (Obj *)bytes_from_u8((-(vv1->len < 0)) ^ vv1->data[v1->val++]);
}

static MUST_CHECK Bytes *and_(const Bytes *vv1, const Bytes *vv2) {
    size_t i, len1, len2, sz;
    int neg1, neg2;
    uint8_t *v1, *v2, *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = neg2 ? len1 : len2;
    if (!sz) return ref_bytes((neg1 & neg2) ? inv_bytes : null_bytes);
    vv = new_bytes();
    v = bnew(vv, sz);
    v1 = vv1->data; v2 = vv2->data;

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
    vv->len = (neg1 & neg2) ? ~sz : sz;
    vv->data = v;
    return vv;
}

static MUST_CHECK Bytes *or_(const Bytes *vv1, const Bytes *vv2) {
    size_t i, len1, len2, sz;
    int neg1, neg2;
    uint8_t *v1, *v2, *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = neg2 ? len2 : len1;
    if (!sz) return ref_bytes((neg1 | neg2) ? inv_bytes : null_bytes);
    vv = new_bytes();
    v = bnew(vv, sz);
    v1 = vv1->data; v2 = vv2->data;

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

    vv->len = (neg1 | neg2) ? ~sz : sz;
    vv->data = v;
    return vv;
}

static MUST_CHECK Bytes *xor_(const Bytes *vv1, const Bytes *vv2) {
    size_t i, len1, len2, sz;
    int neg1, neg2;
    uint8_t *v1, *v2, *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = len1;
    if (!sz) return ref_bytes((neg1 ^ neg2) ? inv_bytes : null_bytes);
    vv = new_bytes();
    v = bnew(vv, sz);
    v1 = vv1->data; v2 = vv2->data;

    for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
    for (; i < len1; i++) v[i] = v1[i];

    vv->len = (neg1 ^ neg2) ? ~sz : sz;
    vv->data = v;
    return vv;
}

static MUST_CHECK Bytes *concat(Bytes *v1, Bytes *v2) {
    Bytes *v;
    uint8_t *s;
    int inv;
    size_t ln, i, len1, len2;

    if (!v1->len) {
        return ref_bytes(v2);
    }
    if (v2->len == 0 || v2->len == ~(ssize_t)0) {
        return ref_bytes(v1);
    }
    len1 = byteslen(v1);
    len2 = byteslen(v2);
    ln = len1 + len2;
    if (ln < len2) err_msg_out_of_memory(); /* overflow */

    v = new_bytes();
    s = bnew(v, ln);
    inv = (v2->len ^ v1->len) < 0;

    memcpy(s, v1->data, len1);
    if (inv) {
        for (i = 0; i < len2; i++) s[i + len1] = ~v2->data[i];
    } else memcpy(s + len1, v2->data, len2);
    v->len = (v1->len < 0) ? ~ln : ln;
    v->data = s;
    return v;
}

static int icmp(Bytes *v1, Bytes *v2) {
    size_t len1 = byteslen(v1), len2 = byteslen(v2);
    int h = memcmp(v1->data, v2->data, (len1 < len2) ? len1 : len2);
    if (h) return h;
    return (len1 > len2) - (len1 < len2);
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1;
    Obj *v;
    Obj *tmp;
    switch (op->op->op) {
    case O_BANK: 
        if (v1->len > 2) return (Obj *)bytes_from_u8(v1->data[2]);
        if (v1->len < ~2) return (Obj *)bytes_from_u8(~v1->data[2]);
        return (Obj *)bytes_from_u8(-(v1->len < 0));
    case O_HIGHER: 
        if (v1->len > 1) return (Obj *)bytes_from_u8(v1->data[1]);
        if (v1->len < ~1) return (Obj *)bytes_from_u8(~v1->data[1]);
        return (Obj *)bytes_from_u8(-(v1->len < 0));
    case O_LOWER: 
        if (v1->len > 0) return (Obj *)bytes_from_u8(v1->data[0]);
        if (v1->len < ~0) return (Obj *)bytes_from_u8(~v1->data[0]);
        return (Obj *)bytes_from_u8(-(v1->len < 0));
    case O_HWORD:
        if (v1->len > 2) return (Obj *)bytes_from_u16(v1->data[1] + (v1->data[2] << 8));
        if (v1->len > 1) return (Obj *)bytes_from_u16(v1->data[1]);
        if (v1->len < ~2) return (Obj *)bytes_from_u16(~(v1->data[1] + (v1->data[2] << 8)));
        if (v1->len < ~1) return (Obj *)bytes_from_u16(~v1->data[1]);
        return (Obj *)bytes_from_u16(-(v1->len < 0));
    case O_WORD:
        if (v1->len > 1) return (Obj *)bytes_from_u16(v1->data[0] + (v1->data[1] << 8));
        if (v1->len > 0) return (Obj *)bytes_from_u16(v1->data[0]);
        if (v1->len < ~1) return (Obj *)bytes_from_u16(~(v1->data[0] + (v1->data[1] << 8)));
        if (v1->len < ~0) return (Obj *)bytes_from_u16(~v1->data[0]);
        return (Obj *)bytes_from_u16(-(v1->len < 0));
    case O_BSWORD:
        if (v1->len > 1) return (Obj *)bytes_from_u16(v1->data[1] + (v1->data[0] << 8));
        if (v1->len > 0) return (Obj *)bytes_from_u16(v1->data[0] << 8);
        if (v1->len < ~1) return (Obj *)bytes_from_u16(~(v1->data[1] + (v1->data[0] << 8)));
        if (v1->len < ~0) return (Obj *)bytes_from_u16(~(v1->data[0] << 8));
        return (Obj *)bytes_from_u16(-(v1->len < 0));
    case O_INV: return invert(v1);
    case O_NEG:
    case O_POS:
    case O_STRING: tmp = (Obj *)int_from_bytes(v1);break;
    default: return obj_oper_error(op);
    }
    op->v1 = tmp;
    v = tmp->obj->calc1(op);
    op->v1 = &v1->v;
    val_destroy(tmp);
    return v;
}

static MUST_CHECK Obj *calc2_bytes(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1, *v2 = (Bytes *)op->v2;
    int val;
    switch (op->op->op) {
    case O_ADD:
    case O_SUB:
    case O_MUL:
    case O_DIV:
    case O_MOD:
    case O_EXP:
        {
            Obj *tmp, *tmp2, *result;
            tmp = (Obj *)int_from_bytes(v1);
            tmp2 = (Obj *)int_from_bytes(v2);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = (Obj *)v1;
            op->v2 = (Obj *)v2;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
    case O_AND: return (Obj *)and_(v1, v2);
    case O_OR: return (Obj *)or_(v1, v2);
    case O_XOR: return (Obj *)xor_(v1, v2);
    case O_LSHIFT:
    case O_RSHIFT:
        {
            Obj *tmp, *tmp2, *result;
            tmp = (Obj *)bits_from_bytes(v1);
            tmp2 = (Obj *)bits_from_bytes(v2);
            op->v1 = tmp;
            op->v2 = tmp2;
            result = tmp->obj->calc2(op);
            op->v1 = (Obj *)v1;
            op->v2 = (Obj *)v2;
            val_destroy(tmp2);
            val_destroy(tmp);
            return result;
        }
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
    case O_CONCAT: return (Obj *)concat(v1, v2);
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            size_t len1 = byteslen(v1), len2 = byteslen(v2), i;
            if (!len1) return (Obj *)ref_bool(true_value);
            if (len1 > len2) return (Obj *)ref_bool(false_value);
            c2 = v2->data;
            e = c2 + len2 - len1;
            if ((v1->len ^ v2->len) < 0) {
                for (;;) {
                    c = (uint8_t *)memchr(c2, ~v1->data[0], e - c2 + 1);
                    if (!c) return (Obj *)ref_bool(false_value);
                    for (i = 1; i < len1; i++) {
                        if (c[i] != (0xff - v1->data[i])) break;
                    }
                    if (i == len1) return (Obj *)ref_bool(true_value);
                    c2 = c + 1;
                }
            } else {
                for (;;) {
                    c = (uint8_t *)memchr(c2, v1->data[0], e - c2 + 1);
                    if (!c) return (Obj *)ref_bool(false_value);
                    if (!memcmp(c, v1->data, len1)) return (Obj *)ref_bool(true_value);
                    c2 = c + 1;
                }
            }
        }
    default: break;
    }
    return obj_oper_error(op);
}

static inline MUST_CHECK Obj *repeat(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1, *v;
    uval_t rep;
    size_t len1 = byteslen(v1);
    Error *err;

    err = op->v2->obj->uval(op->v2, &rep, 8*sizeof(uval_t), op->epoint2);
    if (err) return &err->v;

    if (len1 && rep) {
        uint8_t *s, *s2;
        if (rep == 1) {
            return (Obj *)ref_bytes(v1);
        }
        if (len1 > SSIZE_MAX / rep) err_msg_out_of_memory(); /* overflow */
        v = new_bytes();
        v->data = s2 = s = bnew(v, len1 * rep);
        v->len = 0;
        while (rep--) {
            memcpy(s + v->len, v1->data, len1);
            v->len += len1;
        }
        if (v1->len < 0) v->len = ~v->len;
        return (Obj *)v;
    }
    return (Obj *)ref_bytes((v1->len < 0) ? inv_bytes : null_bytes);
}

static inline MUST_CHECK Obj *slice(Colonlist *v2, oper_t op, size_t ln) {
    uint8_t *p, *p2, inv;
    Bytes *v, *v1 = (Bytes *)op->v1;
    size_t length;
    ival_t offs, end, step;
    Obj *err;

    err = sliceparams(v2, ln, &length, &offs, &end, &step, op->epoint2);
    if (err) return err;

    if (!length) {
        return (Obj *)ref_bytes(null_bytes);
    }
    inv = v1->len < 0;
    if (step == 1 && !inv) {
        if (length == byteslen(v1)) {
            return (Obj *)ref_bytes(v1); /* original bytes */
        }
        if (length == 1) return (Obj *)bytes_from_u8(v1->data[offs]);
        v = new_bytes();
        p = p2 = bnew(v, length);
        memcpy(p2, v1->data + offs, length);
    } else {
        inv = -inv;
        v = new_bytes();
        p = p2 = bnew(v, length);
        while ((end > offs && step > 0) || (end < offs && step < 0)) {
            *p2++ = v1->data[offs] ^ inv;
            offs += step;
        }
    }
    v->len = length;
    v->data = p;
    return (Obj *)v;
}

static inline MUST_CHECK Obj *iindex(oper_t op) {
    uint8_t *p2;
    size_t offs, len1, len2;
    size_t i;
    Bytes *v1 = (Bytes *)op->v1, *v;
    Obj *o2 = op->v2;
    Error *err;
    uint8_t inv;
    Funcargs *args = (Funcargs *)o2;

    if (args->len != 1) {
        err_msg_argnum(args->len, 1, 1, op->epoint2);
        return (Obj *)ref_none();
    }
    o2 = args->val->val;

    len1 = byteslen(v1);

    if (o2->obj == LIST_OBJ) {
        List *list = (List *)o2;
        if (!list->len) {
            return (Obj *)ref_bytes(null_bytes);
        }
        len2 = list->len;
        inv = -(v1->len < 0);
        v = new_bytes();
        v->data = p2 = bnew(v, len2);
        for (i = 0; i < len2; i++) {
            err = indexoffs(list->data[i], len1, &offs, op->epoint2);
            if (err) {
                val_destroy(&v->v);
                return &err->v;
            }
            *p2++ = v1->data[offs] ^ inv;
        }
        v->len = i;
        return (Obj *)v;
    }
    if (o2->obj == COLONLIST_OBJ) {
        return slice((Colonlist *)o2, op, len1);
    }
    err = indexoffs(o2, len1, &offs, op->epoint2);
    if (err) return &err->v;
    inv = -(v1->len < 0);
    return (Obj *)bytes_from_u8(v1->data[offs] ^ inv);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1;
    Obj *o2 = op->v2;
    Obj *tmp;

    if (op->op == &o_INDEX) {
        return iindex(op);
    }
    if (op->op == &o_X) {
        return repeat(op); 
    }
    switch (o2->obj->type) {
    case T_BYTES: return calc2_bytes(op);
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            Obj *result;
            switch (op->op->op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR:
            case O_LSHIFT:
            case O_RSHIFT: tmp = (Obj *)bits_from_bytes(v1); break;
            default: tmp = (Obj *)int_from_bytes(v1);
            }
            op->v1 = tmp;
            result = tmp->obj->calc2(op);
            op->v1 = (Obj *)v1;
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
            return o2->obj->rcalc2(op);
        }
    default: break;
    }
    return obj_oper_error(op);
}

static MUST_CHECK Obj *rcalc2(oper_t op) {
    Bytes *v2 = (Bytes *)op->v2;
    Obj *o1 = op->v1;
    Obj *tmp;
    switch (o1->obj->type) {
    case T_BOOL:
    case T_INT:
    case T_BITS:
    case T_FLOAT:
    case T_CODE:
    case T_ADDRESS:
        {
            Obj *result;
            switch (op->op->op) {
            case O_CONCAT:
            case O_AND:
            case O_OR:
            case O_XOR: tmp = (Obj *)bits_from_bytes(v2); break;
            default: tmp = (Obj *)int_from_bytes(v2);
            }
            op->v2 = tmp;
            result = o1->obj->calc2(op);
            op->v2 =(Obj *)v2;
            val_destroy(tmp);
            return result;
        }
    case T_TUPLE:
    case T_LIST:
    case T_STR:
    case T_NONE:
    case T_ERROR:
        if (op->op != &o_IN) {
            return o1->obj->calc2(op);
        }
    default: break;
    }
    return obj_oper_error(op); 
}

void bytesobj_init(void) {
    obj_init(&obj, T_BYTES, "bytes", sizeof(Bytes));
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

    null_bytes = new_bytes();
    null_bytes->len = 0;
    null_bytes->val[0] = 0;
    null_bytes->data = null_bytes->val;
    inv_bytes = new_bytes();
    inv_bytes->len = ~0;
    inv_bytes->val[0] = 0;
    inv_bytes->data = inv_bytes->val;
}

void bytesobj_destroy(void) {
    int i;
#ifdef DEBUG
    if (null_bytes->v.refcount != 1) fprintf(stderr, "bytes %d\n", null_bytes->v.refcount - 1);
    if (inv_bytes->v.refcount != 1) fprintf(stderr, "invbytes %d\n", inv_bytes->v.refcount - 1);
    for (i = 0; i < 256; i++) {
        if (bytes_value[i] && bytes_value[i]->v.refcount != 1) {
            fprintf(stderr, "bytes[%d] %d\n", i, bytes_value[i]->v.refcount - 1);
        }
    }
#endif

    val_destroy(&null_bytes->v);
    val_destroy(&inv_bytes->v);
    for (i = 0; i < 256; i++) {
        if (bytes_value[i]) val_destroy(&bytes_value[i]->v);
    }
}
