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
#include "bytesobj.h"
#include <string.h>
#include <math.h>
#include "eval.h"
#include "unicode.h"
#include "encoding.h"
#include "variables.h"
#include "arguments.h"

#include "boolobj.h"
#include "floatobj.h"
#include "codeobj.h"
#include "intobj.h"
#include "strobj.h"
#include "bitsobj.h"
#include "listobj.h"
#include "operobj.h"
#include "typeobj.h"
#include "noneobj.h"
#include "error.h"

static Type obj;

Type *BYTES_OBJ = &obj;
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
    case T_BOOL: return (Obj *)bytes_from_u8(((Bool *)v1)->boolean ? 1 : 0);
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

MUST_CHECK Bytes *new_bytes(size_t ln) {
    Bytes *v = (Bytes *)val_alloc(BYTES_OBJ);
    if (ln > sizeof v->val) {
        v->data = (uint8_t *)mallocx(ln);
    } else {
        v->data = v->val;
    }
    return v;
}

static MUST_CHECK Obj *invert(const Bytes *v1) {
    size_t sz;
    sz = byteslen(v1);
    if (sz != 0) {
        Bytes *v = new_bytes(sz);
        v->len = ~v1->len;
        memcpy(v->data, v1->data, sz);
        return &v->v;
    } 
    return (Obj *)ref_bytes((v1->len < 0) ? null_bytes : inv_bytes);
}

static MUST_CHECK Obj *negate(Bytes *v1) {
    size_t i, sz = byteslen(v1);
    Bytes *v;
    if (v1->len == 0) return val_reference(&v1->v);
    if (v1->len < 0) {
        for (i = 0; i < sz; i++) if (v1->data[i] != (uint8_t)~0) break;
        if (i == sz) return NULL;
        v = new_bytes(sz);
        for (i = 0; i < sz; i++) {
            if (v1->data[i] != (uint8_t)~0) {
                v->data[i] = v1->data[i] + 1;
                i++;
                break;
            }
            v->data[i] = 0;
        }
    } else {
        v = new_bytes(sz);
        for (i = 0; i < sz; i++) {
            if (v1->data[i] != 0) {
                v->data[i] = v1->data[i] - 1;
                i++;
                break;
            }
            v->data[i] = ~0;
        }
    }
    for (; i < sz; i++) v->data[i] = v1->data[i];
    v->len = ~v1->len;
    return &v->v;
}

static bool same(const Obj *o1, const Obj *o2) {
    const Bytes *v1 = (const Bytes *)o1, *v2 = (const Bytes *)o2;
    return o2->obj == BYTES_OBJ && v1->len == v2->len && (
            v1->data == v2->data ||
            memcmp(v1->data, v2->data, byteslen(v2)) == 0);
}

static bool to_bool(const Bytes *v1) {
    size_t i, sz;
    if (v1->len < 0) return true;
    sz = byteslen(v1);
    for (i = 0; i < sz; i++) {
        if (v1->data[i] != 0) return true;
    }
    return false;
}

static MUST_CHECK Obj *truth(Obj *o1, enum truth_e type, linepos_t epoint) {
    const Bytes *v1 = (const Bytes *)o1;
    size_t i, sz;
    uint8_t inv;
    switch (type) {
    case TRUTH_ALL:
        if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_BOOL, o1, epoint);
        sz = byteslen(v1);
        inv = (v1->len < 0) ? ~0 : 0;
        for (i = 0; i < sz; i++) {
            if (v1->data[i] == inv) return (Obj *)ref_bool(false_value);
        }
        return (Obj *)ref_bool(true_value);
    case TRUTH_ANY:
        if (diagnostics.strict_bool) err_msg_bool(ERROR_____CANT_BOOL, o1, epoint);
    default:
        return truth_reference(to_bool(v1));
    }
    return DEFAULT_OBJ->truth(o1, type, epoint);
}

static MUST_CHECK Obj *repr(Obj *o1, linepos_t UNUSED(epoint), size_t maxsize) {
    Bytes *v1 = (Bytes *)o1;
    static const char *hex = "0123456789abcdef";
    size_t i, len, len2, sz;
    uint8_t *s, b;
    Str *v;
    sz = byteslen(v1);
    len2 = sz * 2;
    len = (v1->len < 0) ? 9 : 8;
    len += len2;
    if (len < len2 || sz > SIZE_MAX / 2) err_msg_out_of_memory(); /* overflow */
    if (len > maxsize) return NULL;
    v = new_str(len);
    v->chars = len;
    s = v->data;

    memcpy(s, "bytes(", 6);
    s += 6;
    if (v1->len < 0) *s++ = '~';
    *s++ = '$';
    for (i = 0;i < sz; i++) {
        b = v1->data[sz - i - 1];
        *s++ = hex[b >> 4];
        *s++ = hex[b & 0xf];
    }
    *s = ')';
    return &v->v;
}

static MUST_CHECK Error *hash(Obj *o1, int *hs, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
    size_t l = byteslen(v1);
    const uint8_t *s2 = v1->data;
    unsigned int h;
    if (l == 0) {
        *hs = 0;
        return NULL;
    }
    h = *s2 << 7;
    while ((l--) != 0) h = (1000003 * h) ^ *s2++;
    h ^= v1->len;
    *hs = h & ((~(unsigned int)0) >> 1);
    return NULL;
}

MUST_CHECK Obj *bytes_from_str(const Str *v1, linepos_t epoint, enum bytes_mode_e mode) {
    size_t len = v1->len, len2 = (mode == BYTES_MODE_PTEXT || mode == BYTES_MODE_NULL) ? 1 : 0;
    uint8_t *s;
    Bytes *v;
    if (len != 0 || len2 != 0) {
        if (actual_encoding != NULL) {
            int ch;
            if (len < sizeof v->val) len = sizeof v->val;
            if (len == 0) {
                return (Obj *)ref_bytes(null_bytes);
            }
            v = new_bytes(len);
            s = v->data;
            encode_string_init(v1, epoint);
            while ((ch = encode_string()) != EOF) {
                if (len2 >= len) {
                    if (v->val == s) {
                        len = 32;
                        s = (uint8_t *)mallocx(len);
                        memcpy(s, v->val, len2);
                    } else {
                        len += 1024;
                        if (len < 1024) err_msg_out_of_memory(); /* overflow */
                        s = (uint8_t *)reallocx(s, len);
                    }
                }
                switch (mode) {
                case BYTES_MODE_SHIFT_CHECK:
                case BYTES_MODE_SHIFT: if ((ch & 0x80) != 0) encode_error(ERROR___NO_HIGH_BIT); s[len2] = ch & 0x7f; break;
                case BYTES_MODE_SHIFTL: if ((ch & 0x80) != 0) encode_error(ERROR___NO_HIGH_BIT); s[len2] = ch << 1; break;
                case BYTES_MODE_NULL_CHECK:if (ch == 0) {encode_error(ERROR_NO_ZERO_VALUE); ch = 0xff;} s[len2] = ch; break;
                case BYTES_MODE_NULL: if (ch == 0) encode_error(ERROR_NO_ZERO_VALUE); s[len2 - 1] = ch; break;
                case BYTES_MODE_PTEXT:
                case BYTES_MODE_TEXT: s[len2] = ch; break;
                }
                len2++;
            }
            switch (mode) {
            case BYTES_MODE_SHIFT: if (len2 != 0) s[len2 - 1] |= 0x80; else err_msg2(ERROR__BYTES_NEEDED, NULL, epoint); break;
            case BYTES_MODE_SHIFTL: if (len2 != 0) s[len2 - 1] |= 0x01; else err_msg2(ERROR__BYTES_NEEDED, NULL, epoint);break;
            case BYTES_MODE_NULL: s[len2 - 1] = 0x00; break;
            case BYTES_MODE_PTEXT: s[0] = len2 - 1; if (len2 > 256) err_msg2(ERROR____PTEXT_LONG, &len2, epoint); break;
            case BYTES_MODE_SHIFT_CHECK:
            case BYTES_MODE_NULL_CHECK:
            case BYTES_MODE_TEXT: break;
            }
        } else if (v1->chars == 1) {
            uint32_t ch2 = v1->data[0];
            if ((ch2 & 0x80) != 0) utf8in(v1->data, &ch2);
            return (Obj *)bytes_from_uval(ch2, 3);
        } else {
            return (Obj *)new_error(ERROR_BIG_STRING_CO, epoint);
        }
        if (v->val != s) {
            if (len2 <= sizeof v->val) {
                if (len2 != 0) memcpy(v->val, s, len2);
                free(s);
                s = v->val;
            } else if (len2 < len) {
                s = (uint8_t *)reallocx(s, len2);
            }
        }
        if (len2 > SSIZE_MAX) err_msg_out_of_memory(); /* overflow */
        v->len = len2;
        v->data = s;
        return &v->v;
    }
    if (actual_encoding != NULL) {
        if (mode == BYTES_MODE_SHIFT || mode == BYTES_MODE_SHIFTL) err_msg2(ERROR__EMPTY_STRING, NULL, epoint);
    }
    return (Obj *)ref_bytes(null_bytes);
}

MUST_CHECK Bytes *bytes_from_u8(uint8_t i) {
    Bytes *v = bytes_value[i];
    if (v == NULL) {
        v = new_bytes(1);
        v->len = 1;
        v->data[0] = i;
        bytes_value[i] = v;
    }
    return ref_bytes(v);
}

MUST_CHECK Bytes *bytes_from_u16(uint16_t i) {
    Bytes *v = new_bytes(2);
    v->len = 2;
    v->data[0] = i;
    v->data[1] = i >> 8;
    return v;
}

MUST_CHECK Bytes *bytes_from_uval(uval_t i, int bytes) {
    Bytes *v = new_bytes(bytes);
    v->len = bytes;
    switch (bytes) {
    default: v->data[3] = i >> 24;
    case 3: v->data[2] = i >> 16;
    case 2: v->data[1] = i >> 8;
    case 1: v->data[0] = i;
    case 0: break;
    }
    return v;
}

static MUST_CHECK Bytes *bytes_from_bits(const Bits *v1) {
    size_t i, sz, len1;
    uint8_t *d;
    Bytes *v;
    bool inv = (v1->len < 0);

    len1 = v1->bits;
    if (len1 == 0) {
        return ref_bytes(inv ? inv_bytes : null_bytes);
    }
    if (len1 <= 8 && !inv) {
        return bytes_from_u8(v1->data[0]);
    }

    sz = len1 / 8;
    if ((len1 % 8) != 0) sz++;
    if (sz > SSIZE_MAX) err_msg_out_of_memory(); /* overflow */
    v = new_bytes(sz);
    v->len = inv ? ~sz : sz;
    d = v->data;

    len1 = v1->len;
    i = 0;
    if (len1 != 0) {
        bdigit_t b = v1->data[0];
        int bits = 0;
        size_t j = 0;
        while (sz > i) {
            d[i++] = b >> bits;
            if (bits == (8 * sizeof b) - 8) {
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
    bool inv;
    size_t i, j, sz, bits;
    uint8_t *d;
    const digit_t *b;
    Bytes *v;

    switch (v1->len) {
    case -1:
        if (v1->data[0] == 1) return ref_bytes(inv_bytes);
        break;
    case 0:
        return ref_bytes(null_bytes);
    case 1:
        if (v1->data[0] < 256) return bytes_from_u8(v1->data[0]);
    default:
        break;
    }

    inv = (v1->len < 0);
    sz = inv ? -v1->len : v1->len;
    if (sz > SSIZE_MAX / sizeof *b) err_msg_out_of_memory(); /* overflow */
    sz *= sizeof *b;
    v = new_bytes(sz);
    d = v->data;

    b = v1->data;
    if (inv) {
        bool c = (b[0] == 0);
        digit_t b2 = b[0] - 1;
        bits = j = 0;
        for (i = 0; i < sz; i++) {
            d[i] = b2 >> bits;
            if (bits == (8 * sizeof b2) - 8) {
                j++;
                if (c) {
                    c = (b[j] == 0);
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
            if (bits == (8 * sizeof b2) - 8) {
                j++;
                b2 = b[j];
                bits = 0;
            } else bits += 8;
        }
    }

    while (sz != 0 && d[sz - 1] == 0) sz--;
    if (v->val != d) {
        if (sz <= sizeof v->val) {
            if (sz != 0) memcpy(v->val, d, sz);
            free(d);
            d = v->val;
        } else if (sz < i) {
            d = (uint8_t *)reallocx(d, sz);
        }
    }
    v->data = d;
    if (sz > SSIZE_MAX) err_msg_out_of_memory(); /* overflow */
    v->len = inv ? ~sz : sz;

    return v;
}

static bool uval2(Obj *o1, uval_t *uv, unsigned int bits) {
    Bytes *v1 = (Bytes *)o1;
    size_t ln = byteslen(v1);
    uval_t u;
    if (ln > bits / 8) return false;
    u = 0;
    while (ln != 0) u = (u << 8) | v1->data[--ln];
    *uv = (v1->len < 0) ? ~u : u;
    return true;
}

static MUST_CHECK Error *ival(Obj *o1, ival_t *iv, unsigned int bits, linepos_t epoint) {
    Error *v;
    if (uval2(o1, (uval_t *)iv, bits)) return NULL;
    v = new_error(ERROR_____CANT_IVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = val_reference(o1);
    return v;
}

static MUST_CHECK Error *uval(Obj *o1, uval_t *uv, unsigned int bits, linepos_t epoint) {
    Error *v;
    if (uval2(o1, uv, bits)) return NULL;
    v = new_error(ERROR_____CANT_UVAL, epoint);
    v->u.intconv.bits = bits;
    v->u.intconv.val = val_reference(o1);
    return v;
}

MUST_CHECK Obj *float_from_bytes(const Bytes *v1, linepos_t epoint) {
    double d;
    size_t i, len1;
    switch (v1->len) {
    case ~3: d = -1.0 - (double)(v1->data[0] + v1->data[1] * 256 + v1->data[2] * 65536); break;
    case ~2: d = -1.0 - (double)(v1->data[0] + v1->data[1] * 256); break;
    case ~1: d = -1.0 - v1->data[0]; break;
    case ~0: d = -1.0; break;
    case 0: d = 0.0; break;
    case 1: d = v1->data[0]; break;
    case 2: d = (v1->data[0] + v1->data[1] * 256); break;
    case 3: d = (v1->data[0] + v1->data[1] * 256 + v1->data[2] * 65536); break;
    default:
        d = (v1->len < 0) ? -1.0 : 0.0;
        len1 = byteslen(v1);
        for (i = 0; i < len1; i++) {
            if (v1->len < 0) d -= ldexp(v1->data[i], i * 8);
            else d += ldexp(v1->data[i], i * 8);
        }
        return float_from_double(d, epoint);
    }
    return (Obj *)new_float(d);
}

static MUST_CHECK Obj *sign(Obj *o1, linepos_t UNUSED(epoint)) {
    Bytes *v1 = (Bytes *)o1;
    size_t i, sz;
    if (v1->len < 0) return (Obj *)ref_int(minus1_value);
    sz = byteslen(v1);
    for (i = 0; i < sz; i++) {
        if (v1->data[i] != 0) return (Obj *)ref_int(int_value[1]);
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
    uint8_t b;
    const Bytes *vv1 = (Bytes *)v1->data;
    if (v1->val >= byteslen(vv1)) return NULL;
    b = vv1->data[v1->val++];
    return (Obj *)bytes_from_u8((vv1->len < 0) ? ~b : b);
}

static MUST_CHECK Bytes *and_(const Bytes *vv1, const Bytes *vv2) {
    size_t i, len1, len2, sz;
    bool neg1, neg2;
    uint8_t *v1, *v2, *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = neg2 ? len1 : len2;
    if (sz == 0) return ref_bytes((neg1 && neg2) ? inv_bytes : null_bytes);
    vv = new_bytes(sz);
    v = vv->data;
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
    /*if (sz > SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    vv->len = (neg1 && neg2) ? ~sz : sz;
    vv->data = v;
    return vv;
}

static MUST_CHECK Bytes *or_(const Bytes *vv1, const Bytes *vv2) {
    size_t i, len1, len2, sz;
    bool neg1, neg2;
    uint8_t *v1, *v2, *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = neg2 ? len2 : len1;
    if (sz == 0) return ref_bytes((neg1 || neg2) ? inv_bytes : null_bytes);
    vv = new_bytes(sz);
    v = vv->data;
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

    /*if (sz > SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    vv->len = (neg1 || neg2) ? ~sz : sz;
    vv->data = v;
    return vv;
}

static MUST_CHECK Bytes *xor_(const Bytes *vv1, const Bytes *vv2) {
    size_t i, len1, len2, sz;
    bool neg1, neg2;
    uint8_t *v1, *v2, *v;
    Bytes *vv;
    len1 = byteslen(vv1); len2 = byteslen(vv2);

    if (len1 < len2) {
        const Bytes *tmp = vv1; vv1 = vv2; vv2 = tmp;
        i = len1; len1 = len2; len2 = i;
    }
    neg1 = vv1->len < 0; neg2 = vv2->len < 0;

    sz = len1;
    if (sz == 0) return ref_bytes((neg1 != neg2) ? inv_bytes : null_bytes);
    vv = new_bytes(sz);
    v = vv->data;
    v1 = vv1->data; v2 = vv2->data;

    for (i = 0; i < len2; i++) v[i] = v1[i] ^ v2[i];
    for (; i < len1; i++) v[i] = v1[i];

    /*if (sz > SSIZE_MAX) err_msg_out_of_memory();*/ /* overflow */
    vv->len = (neg1 != neg2) ? ~sz : sz;
    vv->data = v;
    return vv;
}

static MUST_CHECK Obj *concat(Bytes *v1, Bytes *v2, linepos_t epoint) {
    Bytes *v;
    uint8_t *s;
    bool inv;
    size_t ln, i, len1, len2;

    if (v1->len == 0) {
        return (Obj *)ref_bytes(v2);
    }
    if (v2->len == 0 || v2->len == ~(ssize_t)0) {
        return (Obj *)ref_bytes(v1);
    }
    len1 = byteslen(v1);
    len2 = byteslen(v2);
    ln = len1 + len2;
    if (ln < len2 || ln > SSIZE_MAX) return (Obj *)new_error(ERROR_OUT_OF_MEMORY, epoint); /* overflow */

    v = new_bytes(ln);
    s = v->data;
    inv = (v2->len ^ v1->len) < 0;

    memcpy(s, v1->data, len1);
    if (inv) {
        for (i = 0; i < len2; i++) s[i + len1] = ~v2->data[i];
    } else memcpy(s + len1, v2->data, len2);
    v->len = (v1->len < 0) ? ~ln : ln;
    v->data = s;
    return &v->v;
}

static int icmp(Bytes *v1, Bytes *v2) {
    size_t len1 = byteslen(v1), len2 = byteslen(v2);
    int h = memcmp(v1->data, v2->data, (len1 < len2) ? len1 : len2);
    if (h != 0) return h;
    if (len1 < len2) return -1;
    return (len1 > len2) ? 1 : 0;
}

static MUST_CHECK Obj *calc1(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1;
    Obj *v;
    Obj *tmp;
    switch (op->op->op) {
    case O_BANK: 
        if (v1->len > 2) return (Obj *)bytes_from_u8(v1->data[2]);
        if (v1->len < ~2) return (Obj *)bytes_from_u8(~v1->data[2]);
        return (Obj *)bytes_from_u8((v1->len < 0) ? 0xff : 0);
    case O_HIGHER: 
        if (v1->len > 1) return (Obj *)bytes_from_u8(v1->data[1]);
        if (v1->len < ~1) return (Obj *)bytes_from_u8(~v1->data[1]);
        return (Obj *)bytes_from_u8((v1->len < 0) ? 0xff : 0);
    case O_LOWER: 
        if (v1->len > 0) return (Obj *)bytes_from_u8(v1->data[0]);
        if (v1->len < ~0) return (Obj *)bytes_from_u8(~v1->data[0]);
        return (Obj *)bytes_from_u8((v1->len < 0) ? 0xff : 0);
    case O_HWORD:
        if (v1->len > 2) return (Obj *)bytes_from_u16(v1->data[1] + (v1->data[2] << 8));
        if (v1->len > 1) return (Obj *)bytes_from_u16(v1->data[1]);
        if (v1->len < ~2) return (Obj *)bytes_from_u16(~(v1->data[1] + (v1->data[2] << 8)));
        if (v1->len < ~1) return (Obj *)bytes_from_u16(~v1->data[1]);
        return (Obj *)bytes_from_u16((v1->len < 0) ? 0xffff : 0);
    case O_WORD:
        if (v1->len > 1) return (Obj *)bytes_from_u16(v1->data[0] + (v1->data[1] << 8));
        if (v1->len > 0) return (Obj *)bytes_from_u16(v1->data[0]);
        if (v1->len < ~1) return (Obj *)bytes_from_u16(~(v1->data[0] + (v1->data[1] << 8)));
        if (v1->len < ~0) return (Obj *)bytes_from_u16(~v1->data[0]);
        return (Obj *)bytes_from_u16((v1->len < 0) ? 0xffff : 0);
    case O_BSWORD:
        if (v1->len > 1) return (Obj *)bytes_from_u16(v1->data[1] + (v1->data[0] << 8));
        if (v1->len > 0) return (Obj *)bytes_from_u16(v1->data[0] << 8);
        if (v1->len < ~1) return (Obj *)bytes_from_u16(~(v1->data[1] + (v1->data[0] << 8)));
        if (v1->len < ~0) return (Obj *)bytes_from_u16(~(v1->data[0] << 8));
        return (Obj *)bytes_from_u16((v1->len < 0) ? 0xffff : 0);
    case O_POS: return val_reference(&v1->v);
    case O_INV: return invert(v1);
    case O_NEG: 
        v = negate(v1); 
        if (v != NULL) return v;
        /* fall through */
    case O_STRING:
        tmp = (Obj *)int_from_bytes(v1);
        op->v1 = tmp;
        v = tmp->obj->calc1(op);
        op->v1 = &v1->v;
        val_destroy(tmp);
        return v;
    case O_LNOT:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return truth_reference(!to_bool(v1));
    default: break;
    }
    return obj_oper_error(op);
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
            op->v1 = &v1->v;
            op->v2 = &v2->v;
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
            op->v1 = &v1->v;
            op->v2 = &v2->v;
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
    case O_CONCAT: return concat(v1, v2, op->epoint3);
    case O_IN:
        {
            const uint8_t *c, *c2, *e;
            size_t len1 = byteslen(v1), len2 = byteslen(v2), i;
            if (len1 == 0) return (Obj *)ref_bool(true_value);
            if (len1 > len2) return (Obj *)ref_bool(false_value);
            c2 = v2->data;
            e = c2 + len2 - len1;
            if ((v1->len ^ v2->len) < 0) {
                for (;;) {
                    c = (uint8_t *)memchr(c2, ~v1->data[0], e - c2 + 1);
                    if (c == NULL) return (Obj *)ref_bool(false_value);
                    for (i = 1; i < len1; i++) {
                        if (c[i] != (0xff - v1->data[i])) break;
                    }
                    if (i == len1) return (Obj *)ref_bool(true_value);
                    c2 = c + 1;
                }
            } else {
                for (;;) {
                    c = (uint8_t *)memchr(c2, v1->data[0], e - c2 + 1);
                    if (c == NULL) return (Obj *)ref_bool(false_value);
                    if (memcmp(c, v1->data, len1) == 0) return (Obj *)ref_bool(true_value);
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

    err = op->v2->obj->uval(op->v2, &rep, 8 * sizeof rep, op->epoint2);
    if (err != NULL) return &err->v;

    if (len1 != 0 && rep != 0) {
        uint8_t *s;
        if (rep == 1) {
            return (Obj *)ref_bytes(v1);
        }
        if (len1 > SSIZE_MAX / rep) return (Obj *)new_error(ERROR_OUT_OF_MEMORY, op->epoint3); /* overflow */
        v = new_bytes(len1 * rep);
        s = v->data;
        v->len = 0;
        while ((rep--) != 0) {
            memcpy(s + v->len, v1->data, len1);
            v->len += len1;
        }
        if (v1->len < 0) v->len = ~v->len;
        return &v->v;
    }
    return (Obj *)ref_bytes((v1->len < 0) ? inv_bytes : null_bytes);
}

static MUST_CHECK Obj *slice(Obj *o1, oper_t op, size_t indx) {
    uint8_t *p2;
    size_t offs2, len1;
    size_t i;
    Bytes *v, *v1 = (Bytes *)o1;
    Obj *o2 = op->v2;
    Error *err;
    uint8_t inv = (v1->len < 0) ? ~0 : 0;
    Funcargs *args = (Funcargs *)o2;
    linepos_t epoint2;

    if (args->len > indx + 1) {
        err_msg_argnum(args->len, 1, indx + 1, op->epoint2);
        return (Obj *)ref_none();
    }
    o2 = args->val[indx].val;
    epoint2 = &args->val[indx].epoint;

    len1 = byteslen(v1);

    if (o2->obj == LIST_OBJ) {
        List *list = (List *)o2;
        size_t len2 = list->len;

        if (len2 == 0) {
            return (Obj *)ref_bytes(null_bytes);
        }
        v = new_bytes(len2);
        p2 = v->data;
        for (i = 0; i < len2; i++) {
            err = indexoffs(list->data[i], len1, &offs2, epoint2);
            if (err != NULL) {
                val_destroy(&v->v);
                return &err->v;
            }
            *p2++ = v1->data[offs2] ^ inv;
        }
        if (i > SSIZE_MAX) err_msg_out_of_memory(); /* overflow */
        v->len = i;
        return &v->v;
    }
    if (o2->obj == COLONLIST_OBJ) {
        size_t length;
        ival_t offs, end, step;

        err = (Error *)sliceparams((Colonlist *)o2, len1, &length, &offs, &end, &step, epoint2);
        if (err != NULL) return &err->v;

        if (length == 0) {
            return (Obj *)ref_bytes(null_bytes);
        }
        if (step == 1 && inv == 0) {
            if (length == byteslen(v1)) {
                return (Obj *)ref_bytes(v1); /* original bytes */
            }
            if (length == 1) return (Obj *)bytes_from_u8(v1->data[offs]);
            v = new_bytes(length);
            memcpy(v->data, v1->data + offs, length);
        } else {
            v = new_bytes(length);
            p2 = v->data;
            while ((end > offs && step > 0) || (end < offs && step < 0)) {
                *p2++ = v1->data[offs] ^ inv;
                offs += step;
            }
        }
        v->len = length;
        return &v->v;
    }
    err = indexoffs(o2, len1, &offs2, epoint2);
    if (err != NULL) return &err->v;
    return (Obj *)bytes_from_u8(v1->data[offs2] ^ inv);
}

static MUST_CHECK Obj *calc2(oper_t op) {
    Bytes *v1 = (Bytes *)op->v1;
    Obj *o2 = op->v2;
    Obj *tmp;

    if (op->op == &o_X) {
        return repeat(op); 
    }
    if (op->op == &o_LAND) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference(to_bool(v1) ? o2 : &v1->v);
    }
    if (op->op == &o_LOR) {
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        return val_reference(to_bool(v1) ? &v1->v : o2);
    }
    switch (o2->obj->type) {
    case T_BYTES: return calc2_bytes(op);
    case T_BOOL:
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        /* fall through */
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
            op->v1 = &v1->v;
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
        if (diagnostics.strict_bool) err_msg_bool_oper(op);
        /* fall through */
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
            op->v2 = &v2->v;
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
    new_type(&obj, T_BYTES, "bytes", sizeof(Bytes));
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
    obj.absolute = absolute;
    obj.len = len;
    obj.getiter = getiter;
    obj.next = next;
    obj.calc1 = calc1;
    obj.calc2 = calc2;
    obj.rcalc2 = rcalc2;
    obj.slice = slice;

    null_bytes = new_bytes(0);
    null_bytes->len = 0;
    null_bytes->data[0] = 0;
    inv_bytes = new_bytes(0);
    inv_bytes->len = ~0;
    inv_bytes->data[0] = 0;
}

void bytesobj_names(void) {
    new_builtin("bytes", val_reference(&BYTES_OBJ->v));
}

void bytesobj_destroy(void) {
    int i;
#ifdef DEBUG
    if (null_bytes->v.refcount != 1) fprintf(stderr, "bytes %" PRIuSIZE "\n", null_bytes->v.refcount - 1);
    if (inv_bytes->v.refcount != 1) fprintf(stderr, "invbytes %" PRIuSIZE "\n", inv_bytes->v.refcount - 1);
    for (i = 0; i < 256; i++) {
        if (bytes_value[i] && bytes_value[i]->v.refcount != 1) {
            fprintf(stderr, "bytes[%d] %" PRIuSIZE "\n", i, bytes_value[i]->v.refcount - 1);
        }
    }
#endif

    val_destroy(&null_bytes->v);
    val_destroy(&inv_bytes->v);
    for (i = 0; i < 256; i++) {
        if (bytes_value[i] != NULL) val_destroy(&bytes_value[i]->v);
    }
}
